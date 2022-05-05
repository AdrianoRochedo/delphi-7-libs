// Demonstracao da Classe MySQL

using System;
using System.Data;
using Rochedo.Data;
using MySQLDriverCS;

public class MainClass
{
    private static void Teste_1() {
        MySQLDataReader r;

		// Uma coneccao sera criada automaticamente
		SQL sql = new MySQL();

        Console.WriteLine("");
		Console.WriteLine("Selecionando ...");
		sql.Command = "SELECT * FROM pessoas";
		r = (MySQLDataReader) sql.Open();
		while (r.Read())
		{
			Console.WriteLine("  " + r.GetString(0) + " - " + r.GetString(1));
		}
        Console.WriteLine("Ok");
        Console.WriteLine("");

        Console.WriteLine("Inserindo ...");
        sql.Command = "INSERT INTO pessoas VALUES ('Pessoa xxxx', 65)";
        if (sql.Exec() == 1)
           Console.WriteLine("Ok");
        else
           Console.WriteLine("Error");

        Console.WriteLine("");
		Console.WriteLine("Selecionando novamente ...");
		sql.Command = "SELECT * FROM pessoas";
		r = (MySQLDataReader) sql.Open();
		while (r.Read())
		{
			Console.WriteLine("  " + r.GetString(0) + " - " + r.GetString(1));
		}
        Console.WriteLine("Ok");

        sql.Dispose();
    }

    private static void Teste_Enum() {
        IDataReader r;

		// Uma coneccao sera criada automaticamente
		SQL sql = new MySQL();

        Console.WriteLine("");
		Console.WriteLine("Selecionando ...");
		sql.Command = "SELECT PJ_Titulo, PJ_Status FROM projetos";
		r = sql.Open();
		while (r.Read())
		{
			Console.WriteLine("  " + r.GetString(0) + " - " + r.GetString(1));
		}
        Console.WriteLine("Ok");
        Console.WriteLine("");
    }

	public static void Main(string[] args)
	{
      //Teste_1();
      Teste_Enum();
	}
}
