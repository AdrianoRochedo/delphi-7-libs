using System;
using System.Collections;
using Rochedo.IO;

namespace Rochedo.Collections {

  /// <summary>
  ///   Implementa uma lista de encadeamento simples não indexável
  /// </summary>
  public class SimpleList : ICollection
  {
     private Item  _First;
     private Item  _Last;
     private int   _Count;

     ~SimpleList()
     {
       Clear();
     }

     // IEnumerable interface ...

     public IEnumerator GetEnumerator()
     {
       return new Enumerator(this);
     }

    // ICollection interface ...

     public void CopyTo(Array array, int index)
     {
       if ((array != null) && (array.Rank != 1))
          throw new ArgumentException("Array multidimensional não suportado"); 

       Item c = _First;
       int UpperBound = array.GetUpperBound(0);
       while ( (c != null) && (index <= UpperBound) )
         {
         array.SetValue(c.aObj, index);
         c = c.Next;
         index ++ ;
         }
     }

     public int Count
     {
       get { return _Count; }
     }

     public bool IsSynchronized
     {
       get { return false; }
     }

     public object SyncRoot
     {
       get { return this; }
     }

     private class Item
     {
       public object aObj;
       public Item Next;

       public Item (object aObj)
       {
         this.aObj = aObj;
         this.Next = null;
       }
     } // Item class

     public void Clear()
     {
       _First = null;
       _Last = null;
       _Count = 0;
     }

     public void Add (object aObj)
     {
       if (_First == null)
          {
          _First = new Item(aObj);
          _Last = _First;
          _Count = 1;
          }
       else
          {
          _Last.Next = new Item(aObj);
          _Last = _Last.Next;
          _Count ++ ;
          }
     }

     private class Enumerator : IEnumerator
     {
        private SimpleList L;
        private Item currentElement;

        internal Enumerator(SimpleList SL)
        {
          this.L = SL;
        }

        public bool MoveNext()
        {
          if (currentElement == null)
             {
             currentElement = L._First;
             return true;
             }
          else
             if (currentElement != L._Last)
                {
                currentElement = currentElement.Next;
                return true;
                }
             else
                return false;
        }

        public void Reset()
        {
          this.currentElement = null;
        }

        public object Current
        {
          get { return currentElement.aObj; }
        }
     } // Enumerator class

  } // SimpleList class

  // --------------------------------------------------------------------------

  /// <summary>
  ///   Implementa uma Lista de Strings com capacidade de salvar e ler arquivos
  /// </summary>
  public class StringList : ITextFileReader, ITextFileWriter
  {
    private System.Collections.ArrayList FList;

    public StringList()
    {
      FList = new System.Collections.ArrayList(10);
    }

    public int Add(string s)
    {
      return FList.Add(s);
    }

    public void Insert(int Pos, string s)
    {
      FList.Insert(Pos, s);
    }

    public void Remove(int Index)
    {
      FList.RemoveAt(Index);
    }

    public void LoadFromFile(string Filename)
    {
      Rochedo.IO.TextFileReader.Read(Filename, this);
    }

    public void SaveToFile(string Filename)
    {
      Rochedo.IO.Utils.SaveToFile(this, Filename);
    }

    public string this [int index]
    {
      get { return (string) FList[index]; }
      set { FList[index] = value; }
    }

    public int Count
    {
      get { return FList.Count; }
    }

    // Declaracoes explicitas de interfaces sao consideradas especiais
    // e nao podem possuir modificadores de acesso, internamente possuem
    // acesso privado por default.

    void ITextFileReader.LineReaded(int LineNumber, string Line)
    {
      Add(Line);
    }

    int ITextFileWriter.getLowIndex()
    {
      return 0;
    }

    int ITextFileWriter.getHighIndex()
    {
      return FList.Count - 1;
    }

    string ITextFileWriter.getString(int index)
    {
      return (string) FList[index];
    }

  } // StringList

  // Ponto de entrada para testes
  public class Test {
    public static void Main()
    {
      Console.WriteLine("StringList:");

          StringList s = new StringList();
          s.Add("1");
          s.Add("2");
          s.SaveToFile("teste.txt");

      Console.WriteLine("StringList: Ok");

      Console.WriteLine("---------------------------");

      Console.WriteLine("SimpleList:");

          SimpleList L = new SimpleList();
          L.Add(1);
          L.Add("dois");
          L.Add(3);

          foreach (object o in L)
            Console.WriteLine( o.ToString() );

          Console.WriteLine("Adicionando mais 10 elementos ...");
          for (int i = 1; i <= 10; i++)
            L.Add(i.ToString());

          foreach (object o in L)
            Console.WriteLine( o.ToString() );

      Console.WriteLine("SimpleList: Ok");
    }
  } // Test

} // Rochedo.Collections


