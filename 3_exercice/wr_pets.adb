with Ada.Direct_IO;
with Ada.Text_IO; use Ada.Text_IO;
with pparaula_wr; use pparaula_wr;

-- Crea un fitxer d'acces directe ('pets.data') a partir dels noms de mascotes introduits.
-- Aquests han d'estar separats per espai i acabats en punt.
  procedure wr_pets is

     package fitxer_paraules is new Ada.Direct_IO(Element_Type => tparaula); use fitxer_paraules;
     origen : OrigenParaules;
     p : tparaula;
     f : fitxer_paraules.File_Type;
     linia, columna : Integer;
  begin
    Put_Line("Introdueix el nom de mascotes separats per espai. Finalizta amb un punt '.':");
     open(origen);
     fitxer_paraules.Create(File => f, Mode => Out_File, Name => "pets.data");

     -- Llegim mascotes fins que introduim un punt
     get(origen, p, linia, columna);
     while not buida(p) loop
        fitxer_paraules.Write(File => f, Item => p);
        get(origen, p, linia, columna);
     end loop;

  end wr_pets;
