<?php 

mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
mysql_select_db("hw12");

function playerid ($name, $rev) {
  if(!ctype_alnum($name))
    die("Invalid player name: " . $name);
  if(!ctype_digit($rev))
    die("Not a number: " . $rev);
   
  $query = 'select id from contestants where name="'.$name.'" && rev='.$rev;
  if($row = mysql_fetch_assoc(mysql_query($query))) 
     return $row['id'];
  else die ("Contestant ".$name.":".$rev." not in contestants.txt");
}

$p0 = playerid ($_REQUEST['player0'], $_REQUEST['player0rev']);
$p1 = playerid ($_REQUEST['player1'], $_REQUEST['player1rev']);

if(!ctype_digit($_REQUEST['rounds'])
   || !ctype_digit($_REQUEST['dead0'])
   || !ctype_digit($_REQUEST['dead1'])
   || !ctype_digit($_REQUEST['zomb0']) 
   || !ctype_digit($_REQUEST['zomb1'])
   || !ctype_digit($_REQUEST['vit0']) 
   || !ctype_digit($_REQUEST['vit1']))
  die ("Bad or missing data arguments");

$query = "insert into matches values (NULL, ";
$query .= $p0 . ", " . $p1 . ", " . $_REQUEST['rounds'] . ", ";
$query .= $_REQUEST['dead0'] . ", " . $_REQUEST['dead1'] . ", ";
$query .= $_REQUEST['zomb0'] . ", " . $_REQUEST['zomb1'] . ", ";
$query .= $_REQUEST['vit0']  . ", " . $_REQUEST['vit1'] . ")";

echo $query;
mysql_query($query);