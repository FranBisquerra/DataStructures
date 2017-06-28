with Ada.Direct_IO;
with Ada.Text_IO; use Ada.Text_IO;
with pparaula_rd; use pparaula_rd;

procedure rd_pets is

  origen_d : OrigenParaules(f_directe);
  p : tparaula;
  num_mascotes : Integer;
begin
  
  open(origen => origen_d, nom => "pets.data");
   
  num_mascotes:= Size(origen_d);
  Put_Line("El nombre de mascotes és " & Size(origen_d)'img & ":");

  for i in 1..num_mascotes loop
    get(origen_d, p, i);
    put(p);
  end loop;

end rd_pets;

