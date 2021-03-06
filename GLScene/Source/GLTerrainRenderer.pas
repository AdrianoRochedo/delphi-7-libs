// GLTerrainRenderer
{: GLScene's brute-force terrain renderer.<p>

   <b>History : </b><font size=-1><ul>
      <li>18/12/01 - EG - Vertex-cache aware stripifier (+10% on GeForce)
      <li>12/08/01 - EG - Completely rewritten handles management
      <li>21/07/01 - EG - Added Notication registration in SetHeightDataSource
      <li>04/03/01 - EG - Completed for first release
	   <li>12/02/01 - EG - Creation
	</ul></font>
}
unit GLTerrainRenderer;

interface

uses Classes, GLScene, GLHeightData, GLTexture, Geometry, GLContext, GLROAMPatch;

type

	// TTerrainRenderer
	//
   {: Basic terrain renderer.<p>
      This renderer uses no sophisticated meshing, it just builds and maintains
      a set of terrain tiles, performs basic visibility culling and renders its
      stuff. You can use it has a base class/sample for more specialized
      terrain renderers.<p>
      The Terrain heightdata is retrieved directly from a THeightDataSource, and
      expressed as z=f(x, y) data. }
	TTerrainRenderer = class (TGLSceneObject)
	   private
	      { Private Declarations }
         FHeightDataSource : THeightDataSource;
         FTileSize : Integer;
         FQualityDistance : Single;
         FLastTriangleCount : Integer;
         FTilesPerTexture : Single;

	   protected
	      { Protected Declarations }
         FTilesHash : array [0..255] of TList;

         procedure SetHeightDataSource(const val : THeightDataSource);
         procedure SetTileSize(const val : Integer);
         procedure SetTilesPerTexture(const val : Single);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure DestroyHandles; override;

         procedure ReleaseAllTiles; dynamic;
         procedure OnTileDestroyed(sender : TObject); virtual;

         {: Render the best fitting tile for tilePos.<p>
            tilePos is in *local* coordinates }
         procedure RenderTile(const tilePos : TAffineVector; eyeDistance : Single;
                              texFactor : Single); virtual;

         function RenderTile2(const tilePos, eyePos : TAffineVector; texFactor : Single) : TGLROAMPatch;

         {: Renders a THeightData as a quad, axis-aligned tile.<p>
            The tile is rendered with a triangle strips. }
         procedure RenderTileAsTriangleStrip(aTile : THeightData;
                       const leftTop, scale, texLeftTop, texScale : TAffineVector;
                       texFactor : Single);
         {: Renders a THeightData as a quad, axis-aligned tile.<p>
            The tile is rendered with a single triangle fans (center to edges). }
         procedure RenderTileAsTriangleFan(aTile : THeightData;
                       const leftTop, scale, texLeftTop, texScale : TAffineVector;
                       texFactor : Single);

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

			procedure BuildList(var rci : TRenderContextInfo); override;

         {: Interpolates height for the given point.<p>
            Expects a point expressed in absolute coordinates. }
         function InterpolatedHeight(const p : TVector) : Single; virtual;

         property LastTriangleCount : Integer read FLastTriangleCount;

	   published
	      { Published Declarations }

         {: Specifies the HeightData provider component. }
         property HeightDataSource : THeightDataSource read FHeightDataSource write SetHeightDataSource;
         {: Size of the terrain tiles.<p>
            Must be a power of two. }
         property TileSize : Integer read FTileSize write SetTileSize default 16;
         {: Number of tiles required for a full texture map. }
         property TilesPerTexture : Single read FTilesPerTexture write SetTilesPerTexture;

         {: Quality distance hint.<p>
            This parameter gives an hint to the terrain renderer at which distance
            the terrain quality can be degraded to favor speed. The distance is
            expressed in absolute coordinates units.<p>
            A value of 0 (default) should be interpreted as "the highest quality". }
         property QualityDistance : Single read FQualityDistance write FQualityDistance;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL12, GLMisc, XOpenGL, VectorLists;

// ------------------
// ------------------ TTerrainRenderer ------------------
// ------------------

// Create
//
constructor TTerrainRenderer.Create(AOwner: TComponent);
var
   i : Integer;
begin
	inherited Create(AOwner);
   for i:=0 to High(FTilesHash) do
      FTilesHash[i]:=TList.Create;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FTileSize:=16;
   FTilesPerTexture:=1;
end;

// Destroy
//
destructor TTerrainRenderer.Destroy;
var
   i : Integer;
begin
   ReleaseAllTiles;
   for i:=0 to High(FTilesHash) do
      FTilesHash[i]:=TList.Create;
	inherited Destroy;
end;

// Notification
//
procedure TTerrainRenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FHeightDataSource) then
      HeightDataSource:=nil;
   inherited;
end;

// DestroyHandles
//
procedure TTerrainRenderer.DestroyHandles;
begin
   inherited;
   ReleaseAllTiles;
   if Assigned(HeightDataSource) then
      HeightDataSource.Clear;
end;

// ReleaseAllTiles
//
procedure TTerrainRenderer.ReleaseAllTiles;
var
   i, k : Integer;
   hd : THeightData;
begin
   for i:=0 to High(FTilesHash) do with FTilesHash[i] do begin
      for k:=0 to Count-1 do begin
         hd:=THeightData(List[k]);
         hd.Release;
      end;
      Clear;
   end;
end;

// OnTileDestroyed
//
procedure TTerrainRenderer.OnTileDestroyed(sender : TObject);
begin
   with sender as THeightData do if ObjectTag<>nil then begin
      ObjectTag.Free;
//      TGLListHandle(Tag).Free;
      ObjectTag:=nil;
   end;
end;

// InterpolatedHeight
//
function TTerrainRenderer.InterpolatedHeight(const p : TVector) : Single;
var
   pLocal : TVector;
begin
   if Assigned(HeightDataSource) then begin
      pLocal:=AbsoluteToLocal(p);
      Result:=(HeightDataSource.InterpolatedHeight(pLocal[0]-TileSize*0.5, pLocal[1]-TileSize*0.5)-128)*Scale.Z;
   end else Result:=0;
end;

// BuildList
//
procedure TTerrainRenderer.BuildList(var rci : TRenderContextInfo);
var
   vEye : TVector;
   tilePos, absTilePos, observer : TAffineVector;
   delta, n : Integer;
   f, tileRadius, texFactor : Single;
   patch, prevPatch : TGLROAMPatch;
   vertices : TAffineVectorList;
   vertexIndices : TIntegerList;
   texPoints : TTexPointList;
   patchList, rowList, prevRow, buf : TList;
begin
   texFactor:=1/(TilesPerTexture*TileSize);
   if csDesigning in ComponentState then Exit;
   if HeightDataSource=nil then Exit;
   // first project eye position into heightdata coordinates
   vEye:=VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
   SetVector(observer, vEye);
   vEye[0]:=Round(vEye[0]/TileSize-0.5)*TileSize+TileSize*0.5;
   vEye[1]:=Round(vEye[1]/TileSize-0.5)*TileSize+TileSize*0.5;
   tileRadius:=Sqrt(Sqr(TileSize*0.5*Scale.X)+Sqr(TileSize*0.5*Scale.Y)+Sqr(128*Scale.Z))*1.5;
   // mark all tiles as unused
//   for n:=0 to FTiles.Count-1 do
//      THeightData(FTiles[n]).ObjectTag:=nil;
   FLastTriangleCount:=0;
   // now, we render a quad grid centered on eye position
   SetVector(tilePos, vEye);
   delta:=TileSize;
   tilePos[2]:=0;
   f:=(rci.rcci.farClippingDistance+tileRadius)/Scale.X;
   f:=Round(f/TileSize+0.5)*TileSize;
   tilePos[1]:=vEye[1]-f;

   SetROAMTrianglesCapacity(900000);
   patchList:=TList.Create;
   rowList:=TList.Create;
   prevRow:=TList.Create;

   while tilePos[1]<=vEye[1]+f do begin
      tilePos[0]:=vEye[0]-f;
      prevPatch:=nil;
      n:=0;
      while tilePos[0]<=vEye[0]+f do begin
         absTilePos:=VectorAdd(tilePos, delta*0.5);
         absTilePos:=VectorTransform(absTilePos, AbsoluteMatrix);
         if not IsVolumeClipped(absTilePos, tileRadius, rci.rcci) then begin
//            RenderTile(tilePos, VectorDistance(absTilePos, AffineVectorMake(rci.cameraPosition)), texFactor);
            patch:=RenderTile2(tilePos, observer, texFactor);
            if Assigned(prevPatch) then
               patch.ConnectToTheLeft(prevPatch);
            if prevRow.Count>n then
               if prevRow.List[n]<>nil then
                  patch.ConnectToTheTop(TGLROAMPatch(prevRow.List[n]));
            prevPatch:=patch;
            patchList.Add(patch);
            rowList.Add(patch);
         end else begin
            prevPatch:=nil;
            rowList.Add(nil);
         end;
         tilePos[0]:=tilePos[0]+delta;
         Inc(n);
      end;
      tilePos[1]:=tilePos[1]+delta;
      buf:=prevRow;
      prevRow:=rowList;
      rowList:=buf;
      rowList.Count:=0;
   end;

   vertices:=TAffineVectorList.Create;
   vertices.Capacity:=Sqr(TileSize+1)*2;
   texPoints:=TTexPointList.Create;
   texPoints.Capacity:=Sqr(TileSize+1)*2;
   vertexIndices:=TIntegerList.Create;

   for n:=0 to patchList.Count-1 do begin
      patch:=TGLROAMPatch(patchList[n]);
      patch.Tesselate(10);
   end;

   glEnableClientState(GL_VERTEX_ARRAY);
   xglEnableClientState(GL_TEXTURE_COORD_ARRAY);

   xglTexCoordPointer(2, GL_FLOAT, 0, texPoints.List);
   glVertexPointer(3, GL_FLOAT, 0, vertices.List);

   for n:=0 to patchList.Count-1 do begin
      patch:=TGLROAMPatch(patchList[n]);

      texPoints.Count:=0;
      vertexIndices.Count:=0;
      vertices.Count:=0;
      patch.Render(vertices, vertexIndices, texPoints);
      vertices.Translate(patch.VertexOffset);
{      glBegin(GL_TRIANGLES);
      for i:=0 to vertexIndices.Count-1 do begin
         k:=vertexIndices.List[i];
         xglTexCoord2fv(@texPoints.List[k]);
         glVertex3fv(@vertices.List[k]);
      end;
      glEnd; }
      glLockArraysEXT(0, vertices.Count);
      glDrawElements(GL_TRIANGLES, vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.List);
      glUnlockArraysEXT;

      FLastTriangleCount:=FLastTriangleCount+vertexIndices.Count div 3;
   end;

   texPoints.Free;
   vertexIndices.Free;
   vertices.Free;

   rowList.Free;
   prevRow.Free;
   patchList.Free;

   // release all unused tiles
{   for n:=FTiles.Count-1 downto 0 do
      if THeightData(FTiles[n]).ObjectTag=nil then begin
         THeightData(FTiles[n]).Release;
         FTiles.Delete(n);
      end;}
end;

function TTerrainRenderer.RenderTile2(const tilePos, eyePos : TAffineVector; texFactor : Single) : TGLROAMPatch;
var
   i, hash : Integer;
   hd, tile : THeightData;
   xLeft, yTop : Integer;
   patch : TGLROAMPatch;
//   vertices : TAffineVectorList;
//   vertexIndices : TIntegerList;
//   texPoints : TTexPointList;
begin
   xLeft:=Round(tilePos[0]/(TileSize)-0.5)*(TileSize);
   yTop:=Round(tilePos[1]/(TileSize)-0.5)*(TileSize);
   // is the tile already in our list?
   hash:=( xLeft+(xLeft shr 8)+(xLeft shr 16)
          +yTop+(yTop shr 8)+(yTop shr 16)) and 255;
   tile:=nil;
   patch:=nil;
   with FTilesHash[hash] do begin
      for i:=0 to Count-1 do begin
         hd:=THeightData(List[i]);
         if (hd.XLeft=xLeft) and (hd.YTop=yTop) then begin
            tile:=hd;
            patch:=TGLROAMPatch(tile.ObjectTag);
            Break;
         end;
      end;
   end;
   // if not, request it
   if not Assigned(tile) then begin
      tile:=HeightDataSource.GetData(xLeft, yTop, TileSize+1, hdtByte);
      tile.OnDestroy:=OnTileDestroyed;
      tile.DataType:=hdtWord;
      FTilesHash[hash].Add(tile);
      // spawn ROAM patch
      patch:=TGLROAMPatch.Create;
      tile.ObjectTag:=patch;
      patch.HeightData:=tile;
      patch.VertexScale:=XYZVector;
      patch.VertexOffset:=tilePos;
      patch.TextureScale:=AffineVectorMake(texFactor, texFactor, texFactor);
      patch.TextureOffset:=AffineVectorMake(xLeft*texFactor, 1-yTop*texFactor, 0);
      patch.ComputeVariance(11);
   end;
   PAffineIntVector(@patch.ObserverPosition)[0]:=Round(eyePos[0]-xLeft-TileSize shr 1);
   PAffineIntVector(@patch.ObserverPosition)[1]:=Round(eyePos[1]-yTop-TileSize shr 1);
   PAffineIntVector(@patch.ObserverPosition)[2]:=Round(eyePos[2]);
   patch.ResetTessellation;
   Result:=patch;
end;

// RenderTile
//
procedure TTerrainRenderer.RenderTile(const tilePos : TAffineVector;
                                      eyeDistance : Single; texFactor : Single);
var
   i, hash : Integer;
   hd, tile : THeightData;
   xLeft, yTop : Integer;
   listHandle : TGLListHandle;
begin
   xLeft:=Round(tilePos[0]/(TileSize-3)-0.5)*(TileSize-3);
   yTop:=Round(tilePos[1]/(TileSize-3)-0.5)*(TileSize-3);
   // is the tile already in our list?
   hash:=( xLeft+(xLeft shr 8)+(xLeft shr 16)
          +yTop+(yTop shr 8)+(yTop shr 16)) and 255;
   tile:=nil;
   with FTilesHash[hash] do begin
      for i:=0 to Count-1 do begin
         hd:=THeightData(List[i]);
         if (hd.XLeft=xLeft) and (hd.YTop=yTop) then begin
            tile:=hd;
            Break;
         end;
      end;
   end;
   // if not, request it
   if not Assigned(tile) then begin
      tile:=HeightDataSource.GetData(xLeft, yTop, TileSize, hdtByte);
      tile.OnDestroy:=OnTileDestroyed;
      FTilesHash[hash].Add(tile);
   end;
   // build/rebuild list
   if (QualityDistance>0) and (tile.Tag<>0) then begin
      if (((eyeDistance<QualityDistance) and (tile.Tag2=0))
          or ((eyeDistance>=QualityDistance) and (tile.Tag2=1))) then begin
         glDeleteLists(tile.Tag, 1);
         tile.Tag:=0;
      end;
   end;
   if tile.Tag=0 then begin
      listHandle:=TGLListHandle.Create;
      listHandle.AllocateHandle;
      tile.Tag:=Integer(listHandle);
      glNewList(listHandle.Handle, GL_COMPILE);
      if (eyeDistance<QualityDistance) or (QualityDistance<=0) then begin
         tile.Tag2:=1;
         RenderTileAsTriangleStrip(tile,
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1),
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1), texFactor);
      end else begin
         tile.Tag2:=0;
         RenderTileAsTriangleFan(tile,
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1),
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1), texFactor);
      end;
      glEndList;
   end else listHandle:=TGLListHandle(tile.Tag);
   // start rendering
   glCallList(listHandle.Handle);
   if tile.Tag2=1 then
      Inc(FLastTriangleCount, 2*(tile.Size-3)*(tile.Size-3))
   else Inc(FLastTriangleCount, 4*(tile.Size-2)-2);
   // mark tile as used
   tile.ObjectTag:=Pointer(1);
end;

// RenderTileAsTriangleStrip
//
procedure TTerrainRenderer.RenderTileAsTriangleStrip(aTile : THeightData;
              const leftTop, scale, texLeftTop, texScale : TAffineVector;
              texFactor : Single);
var
   x, y, dx, ex : Integer;
   pTop, pBottom : TAffineVector;
   bottomRow, topRow : GLHeightData.PByteArray;

   procedure IssueVertex(const n, v : TAffineVector);
   begin
//      glNormal3fv(@n);
      xglTexCoord2f(v[0]*texFactor, -v[1]*texFactor);
      glVertex3fv(@v);
   end;

begin
   // to optimize : normals calculation is slooooowwww
   // the cacheing takes care of it, but still...
   for y:=1 to aTile.Size-3 do begin
      pTop[1]:=leftTop[1]+y*scale[1];
      pBottom[1]:=leftTop[1]+(y+1)*scale[1];
      bottomRow:=aTile.ByteRaster[y+1];
      topRow:=aTile.ByteRaster[y];
      if (y and 1)=0 then begin
         x:=aTile.Size-2;
         ex:=0;
         dx:=-1;
      end else begin
         x:=1;
         ex:=aTile.Size-1;
         dx:=1;
      end;
      // Strips direction is reversed from one strip to another,
      // this increases vertex coherency (10% faster on GeForce)
      glBegin(GL_TRIANGLE_STRIP);
      while x<>ex do begin
         pTop[0]:=leftTop[0]+x*scale[0];
         pBottom[0]:=pTop[0];
         pBottom[2]:=(bottomRow[x]-128)*scale[2];
         pTop[2]:=(topRow[x]-128)*scale[2];
         if dx=1 then begin
            IssueVertex(aTile.Normal(x, y+1, scale), pBottom);
            IssueVertex(aTile.Normal(x, y, scale), pTop);
            Inc(x);
         end else begin
            IssueVertex(aTile.Normal(x, y, scale), pTop);
            IssueVertex(aTile.Normal(x, y+1, scale), pBottom);
            Dec(x);
         end;
      end;
      glEnd;
   end;
end;

// RenderTileAsTriangleFan
//
procedure TTerrainRenderer.RenderTileAsTriangleFan(aTile : THeightData;
              const leftTop, scale, texLeftTop, texScale : TAffineVector;
              texFactor : Single);
var
   x, y : Integer;
   p : TAffineVector;
   n : TAffineVector;
   row : GLHeightData.PByteArray;
begin
   // to optimize : normals calculation is slooooowwww
   // the cacheing takes care of it, but still...
   x:=(aTile.Size-2) div 2;
   p[0]:=leftTop[0]+x*scale[0];
   p[1]:=leftTop[1]+x*scale[1];
   p[2]:=(aTile.ByteRaster[x][x]-128)*scale[2];
   n:=aTile.Normal(x, x, scale);
   glBegin(GL_TRIANGLE_FAN);
      glNormal3fv(@n);
      xglTexCoord2f(p[0]*texFactor, -p[1]*texFactor);
      glVertex3fv(@p);
      p[1]:=leftTop[1]+1*scale[0];
      row:=aTile.ByteRaster[1];
      for x:=1 to aTile.Size-2 do begin
         p[0]:=leftTop[0]+x*scale[0];
         p[2]:=(row[x]-128)*scale[2];
         n:=aTile.Normal(x, 1, scale);
         glNormal3fv(@n);
         xglTexCoord2f(p[0]*texFactor, -p[1]*texFactor);
         glVertex3fv(@p);
      end;
      for y:=2 to aTile.Size-2 do begin
         p[1]:=leftTop[1]+y*scale[1];
         p[2]:=(aTile.ByteRaster[y][aTile.Size-2]-128)*scale[2];
         n:=aTile.Normal(aTile.Size-2, y, scale);
         glNormal3fv(@n);
         xglTexCoord2f(p[0]*texFactor, -p[1]*texFactor);
         glVertex3fv(@p);
      end;
      row:=aTile.ByteRaster[aTile.Size-2];
      for x:=aTile.Size-3 downto 1 do begin
         p[0]:=leftTop[0]+x*scale[0];
         p[2]:=(row[x]-128)*scale[2];
         n:=aTile.Normal(x, aTile.Size-2, scale);
         glNormal3fv(@n);
         xglTexCoord2f(p[0]*texFactor, -p[1]*texFactor);
         glVertex3fv(@p);
      end;
      for y:=aTile.Size-3 downto 1 do begin
         p[1]:=leftTop[1]+y*scale[1];
         p[2]:=(aTile.ByteRaster[y][1]-128)*scale[2];
         n:=aTile.Normal(1, y, scale);
         glNormal3fv(@n);
         xglTexCoord2f(p[0]*texFactor, -p[1]*texFactor);
         glVertex3fv(@p);
      end;
   glEnd;
end;

// SetHeightDataSource
//
procedure TTerrainRenderer.SetHeightDataSource(const val : THeightDataSource);
begin
   if FHeightDataSource<>val then begin
      if Assigned(FHeightDataSource) then begin
         FHeightDataSource.RemoveFreeNotification(Self);
         ReleaseAllTiles;
         FHeightDataSource.Clear;
      end;
      FHeightDataSource:=val;
      if Assigned(FHeightDataSource) then
         FHeightDataSource.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetTileSize
//
procedure TTerrainRenderer.SetTileSize(const val : Integer);
begin
   if val<>FTileSize then begin
      if val<8 then
         FTileSize:=8
      else FTileSize:=RoundUpToPowerOf2(val);
      ReleaseAllTiles;
      StructureChanged;
   end;
end;

// SetTilesPerTexture
//
procedure TTerrainRenderer.SetTilesPerTexture(const val : Single);
begin
   if val<>FTilesPerTexture then begin
      FTilesPerTexture:=val;
      StructureChanged;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TTerrainRenderer);

end.

