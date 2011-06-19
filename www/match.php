<?php 

mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
mysql_select_db("hw12");

$query = "select player0.name as p0, player0.rev as r0, ";
$query .= "player1.name as p1, player1.rev as r1, count(matches.id) as num "; 
$query .= "from (contestants as player0 join contestants as player1) ";
$query .= "left join matches on player0.id = matches.player0 && ";
$query .=                      "player1.id = matches.player1 ";
$query .= "where player0.active = 1 && player1.active = 1 ";
$query .= "group by player0.id, player1.id order by count(matches.id) ";
$query .= "limit 0, 100 ";
$result = mysql_query($query);

while ($row = mysql_fetch_assoc($result)) {
  echo $row['p0'] . ":" . $row['r0'] . "::";
  echo $row['p1'] . ":" . $row['r1'] . ":::";
  echo $row['num'] . "\n"; 
}