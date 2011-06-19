<?php 

mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
mysql_select_db("hw12");

if($_REQUEST['outcome'] == 'tie') 
  { $tie = 1; $onewin = 0; $twowin = 0; }
else if($_REQUEST['outcome'] == 'win1') 
  { $tie = 0; $onewin = 1; $twowin = 0; }
else if($_REQUEST['outcome'] == 'win2')
  { $tie = 0; $onewin = 0; $twowin = 1; }
else die("Bad outcome: " ^ $_REQUEST['outcome']);

$p1 = $_REQUEST['player1'];
if(!ctype_alnum($_REQUEST['player1']))
  die("Bad player1");

$p1r = $_REQUEST['player1rev'];
if(!ctype_digit($p1r)) 
  die("Bad player1rev");

$p2 = $_REQUEST['player2'];
if(!ctype_alnum($p2)) 
  die("Bad player2");

$p2r = $_REQUEST['player2rev'];
if(!ctype_digit($p2r)) 
  die("Bad player2rev");

$query = 'select id from players where name="' . $p1 . '" && rev = ' . $p1r;
$result = mysql_query($query);

if($row = mysql_fetch_assoc($result)) 
  $p1id = $row['id'];
else 
  die("Bad fetch1: " . $query);

$result = mysql_query('select id from players where name="' . $p2 . '" && rev = ' . $p2r);

if($row = mysql_fetch_assoc($result))
  $p2id = $row['id'];
else
  die("Bad fetch2");

mysql_query('insert into rounds values(NULL, ' . $p1id . ', ' . $p2id . ', ' . $tie . ', ' . $onewin . ', ' . $twowin . ')'); 


