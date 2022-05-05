using System;
using System.IO;

namespace Rochedo.IO {

  /// <summary>
  ///   <para>A implementa��o desta interface permite a classe gerenciar/armazenar</para>
  ///   <para>"strings" lidas de um arquivo texto.</para>
  /// </summary>
  public interface ITextFileReader {
    void LineReaded(int LineNumber, string Line);
  }

  /// <summary>
  ///   <para>A implementa��o desta interface automatiza o processo de salvamento</para>
  ///   <para>em arquivo texto das "strings" gerenciadas pela instancia.</para>
  /// </summary>
  public interface ITextFileWriter {
    int getLowIndex();
    int getHighIndex();
    string getString(int index);
  }

  // --------------------------------------------------------------------------

  /// <summary>
  ///   <para>L� um arquivo passando as linhas lidas para o objeto</para>
  ///   <para>implementador da interface "ITextFileReader".</para>
  /// </summary>
  /// <example>
  /// <code lang="C#">
  ///   TextFileReader.Read("teste.txt", this);
  /// </code>
  /// <code lang="Delphi">
  ///   TextFileReader.Read('teste.txt', self);
  /// </code>
  /// </example>
  public class TextFileReader
  {
    /// <summary>
    ///   <para>Faz a leitura de um arquivo passando a responsabilidade do</para>
    ///   <para>armazenamento das linhas para a instancia implementadora de "ITextFileReader"</para>
    /// </summary>
    public static void Read(string FileName, ITextFileReader aObject)
    {
      System.IO.Stream stream = new System.IO.FileStream(
        FileName, FileMode.Open, FileAccess.Read);

      try {
        ReadText(stream, aObject);
      }
      finally {
        stream.Close();
      }
    } // Read

    private static void ReadText(System.IO.Stream stream, ITextFileReader aObject)
    {
      StreamReader sr = new StreamReader(stream);
      try {
        // Leitura do conteudo
        int i = 0; string s;
        while ( (s = sr.ReadLine()) != null ) {
          i++;
          aObject.LineReaded(i, s);
        }
      }
      finally {
        sr.Close();
      }
    } // ReadText

  } // class TextFileReader

  // --------------------------------------------------------------------------

  /// <summary>
  ///   <para>Fornece acesso a v�rios m�todos �teis.</para>
  /// </summary>
  /// <remarks>
  ///   <para>Todos os m�todos s�o est�ticos.</para>
  /// </remarks>
  public class Utils {

    /// <summary>
    ///   <para>SaveToFile(System.Object aObject, string Filename)</para>
    ///   <para>Salva a instancia em um arquivo texto</para>
    /// </summary>
    /// <param name="aObject">Objeto a ser salvo</param>
    /// <param name="Filename">Nome do arquivo</param>
    /// <remarks>
    ///   <para>Se o objeto n�o implementar a interface "ITextFileWriter",</para>
    ///   <para>o m�todo invocado ser� "ToString()" para obten��o do conte�do.</para>
    ///   <para>Se ele implementar esta interface, ser�o chamados os m�todos</para>
    ///   <para>da interface para obten��o das "strings" que formar�o o arquivo.</para>
    /// </remarks>
    /// <example>
    /// <code lang="C#">
    ///   Utils.SaveToFile(this, "teste.txt");
    /// </code>
    /// </example>
    public static void SaveToFile(System.Object aObject, string Filename)
    {
      FileStream stream = new FileStream(Filename, FileMode.Create,
                                                   FileAccess.Write);
      try {
        SaveStream(stream, aObject);
      }
      finally {
        stream.Close();
      }
    }

    private static void SaveStream(FileStream stream, System.Object aObject)
    {
      StreamWriter sw = new StreamWriter(stream);
      try {
        if (aObject is ITextFileWriter) {
           ITextFileWriter o = aObject as ITextFileWriter;
           int L = o.getLowIndex();
           int H = o.getHighIndex();
           for (int i = L; i <= H; i++)
             sw.WriteLine( o.getString(i) );
           }  
        else
           sw.Write( aObject.ToString() );
      }
      finally {
        sw.Close();
      }
    }

  } // class Utils

} // namespace Rochedo.IO


