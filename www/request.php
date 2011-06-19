<?php 

mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
mysql_select_db("hw12");
$result = mysql_query("select player1.name as p1, player1.rev as p1r, player2.name as p2, player2.rev as p2r from (players as player1 join players as player2) left join rounds on rounds.player1 = player1.id && rounds.player2 = player2.id group by player1.id, player2.id order by count(rounds.id) limit 0, 30");

$which = rand(0, 29);

while ($row = mysql_fetch_assoc($result)) {
  if($which == 0)
    echo $row['p1'] . ":" . $row['p1r'] . "  " . $row['p2'] . ":" . $row['p2r']; 
  $which = $which - 1;
}
