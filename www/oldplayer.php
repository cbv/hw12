<?php
  mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
  mysql_select_db("hw12");

  $id = $_REQUEST['id'];
  if(!ctype_digit($id)) die ("Not a number: " . $id);

  $result = mysql_query('select * from contestants where id = ' . $id);
  if ($row = mysql_fetch_assoc($result)) {
    $active = $row['active'];
    $name   = $row['name'];
    $rev    = $row['rev'];
  } else die ("Not a real id: " . $id);

  $player = $name . ":" . $rev;
?>

<html>
 <head><title>Info page for <?php echo $player ?></title></head>
<body>
<h1>Planeswalker "<?php echo $player ?>"</h1>

(<a href="/arena/">home</a>)

<h2>As player 0</h2>

<table>
 <tr>
  <td style="width: 200px; background: ada">Versus</td>
  <td style="width: 100px; background: ada">My dead</td>
  <td style="width: 100px; background: ada">Their dead</td>
  <td style="width: 100px; background: ada">Rounds</td>
  <td style="width: 100px; background: ada">Score</td>
 </tr>
<?php 
  $query = "select * from matches join contestants ";
  $query .= "on player1 = contestants.id where player0 = " . $id . " ";
  $query .= "group by player0, player1 order by player0, player1";
  $result = mysql_query($query);
  $totalscore = 0;
  $totalopp = 0;
  $activescore = 0;
  $activeopp = 0;
  $bool = 1;
  while($row = mysql_fetch_assoc($result)) {
     $diff = $row['zomb0'] + $row['dead0'] - $row['zomb1'] - $row['dead1'];
     $rounds = ceil($row['rounds'] / 2);

     # Calculate score
     if($diff > 0) 
       $score = 0;
     else if($diff == 0)
       $score = 1;
     else if($rounds == 100000) 
       $score = 2;
     else
       $score = 6;

     if($score == 0)
       $xtra = "; color: f00; font-weight: bold";
     else 
       $xtra = "";

     if($bool) {
       $td = '<td style="background: fff' . $xtra . '">';
       $bool = 0;
     } else {
       $td = '<td style="background: ddd' . $xtra . '">'; 
       $bool = 1;
     }   

     $totalscore = $totalscore + $score;
     $totalopp = $totalopp + 1;

     if($row['active'] == "1") {
       $activescore = $activescore + $score;
       $activeopp = $activeopp + 1;
     }

     echo " <tr>\n";
     echo '  ' . $td . $row['name'] . ":" . $row['rev'] . "</td>\n";
     echo "  " . $td . ($row['zomb0'] + $row['dead0']) . "</td>\n";
     echo "  " . $td . ($row['zomb1'] + $row['dead1']) . "</td>\n";
     echo "  " . $td . $rounds . "</td>\n";
     echo "  " . $td . $score . "</td>\n";
     echo " </tr>\n";
  } 

  if($totalopp > 0) 
    $totalavg = (0.0 + $totalscore) / $totalopp;
  else
    $totalavg = 0.0;

  if($activeopp > 0)
    $activeavg = (0.0 + $activescore) / $activeopp;
  else
    $activeacg = 0.0;

?></table>
<span style="font-style: italic">
Average score against recorded opponents: <?php echo $totalavg ?><br/>
Average score against active opponents: <?php echo $activeavg ?></br>
</span>

<h2>As player 1</h2>

<table>
 <tr>
  <td style="width: 200px; background: ada">Versus</td>
  <td style="width: 100px; background: ada">My dead</td>
  <td style="width: 100px; background: ada">Their dead</td>
  <td style="width: 100px; background: ada">Rounds</td>
  <td style="width: 100px; background: ada">Score</td>
 </tr>
<?php 
  $query = "select * from matches join contestants ";
  $query .= "on player0 = contestants.id where player1 = " . $id . " ";
  $query .= "group by player0, player1 order by player0, player1";
  $result = mysql_query($query);
  $totalscore = 0;
  $totalopp = 0;
  $activescore = 0;
  $activeopp = 0;
  $bool = 1;
  while($row = mysql_fetch_assoc($result)) {
     $diff = $row['zomb0'] + $row['dead0'] - $row['zomb1'] - $row['dead1'];
     $rounds = ceil($row['rounds'] / 2);

     # Calculate score
     if($diff < 0) 
       $score = 0;
     else if($diff == 0)
       $score = 1;
     else if($rounds == 100000) 
       $score = 2;
     else
       $score = 6;

     # Score-specific formatting
     if($score == 0)
       $xtra = "; color: f00; font-weight: bold";
     else 
       $xtra = "";

     # Alternating effect
     if($bool) {
       $td = '<td style="background: fff' . $xtra . '">';
       $bool = 0;
     } else {
       $td = '<td style="background: ddd' . $xtra . '">'; 
       $bool = 1;
     }   

     $totalscore = $totalscore + $score;
     $totalopp = $totalopp + 1;

     if($row['active'] == "1") {
       $activescore = $activescore + $score;
       $activeopp = $activeopp + 1;
     }

     echo " <tr>\n";
     echo "  " . $td . $row['name'] . ":" . $row['rev'] . "</td>\n";
     echo "  " . $td . ($row['zomb1'] + $row['dead1']) . "</td>\n";
     echo "  " . $td . ($row['zomb0'] + $row['dead0']) . "</td>\n";
     echo "  " . $td . $rounds . "</td>\n";
     echo "  " . $td . $score . "</td>\n";
     echo " </tr>\n";
  } 

  if($totalopp > 0) 
    $totalavg = (0.0 + $totalscore) / $totalopp;
  else
    $totalavg = 0.0;

  if($activeopp > 0)
    $activeavg = (0.0 + $activescore) / $activeopp;
  else
    $activeacg = 0.0;

?></table>
<span style="font-style: italic">
Average score against recorded opponents: <?php echo $totalavg ?><br/>
Average score against active opponents: <?php echo $activeavg ?></br>
</span>

</body>
</html>