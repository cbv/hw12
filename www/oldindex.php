<?php
  mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
  mysql_select_db("hw12");

  # Base query
  function query($arg) {
     # Definitions
     $tie = "(dead0 + zomb0 = dead1 + zomb1)";
     $win0 = "(dead0 + zomb0 < dead1 + zomb1)";
     $win1 = "(dead0 + zomb0 > dead1 + zomb1)";
     $ko0 = "(rounds < 200000 && " . $win0 . ")";
     $ko1 = "(rounds < 200000 && " . $win1 . ")";
     $score0 = "(2 * " . $win0 . " + 4 * " . $ko0 . " + " . $tie . ")"; 
     $score1 = "(2 * " . $win1 . " + 4 * " . $ko1 . " + " . $tie . ")"; 

     $q = "select contestants.id as player, name, rev, active, ";
     $q .= "sum" . $win0 . " as win0, sum" . $win1 . " as win1, ";
     $q .= "sum" . $tie . " as tie, "; 
     $q .= "sum" . $ko0 . " as ko0, sum" . $ko1 . " as ko1, ";
     $q .= "avg" . $score0 . " as score0, avg " . $score1 . " as score1 ";
     $q .= "from contestants left join matches ";
     $q .= "on contestants.id = matches.player" . $arg . " ";
     $q .= "group by contestants.id order by avg";
     if ($arg == "0")
       $q .= $score0;
     else $q .= $score1;
     $q .= " desc";
     return $q;
  }
?>

<html>
 <head><title>CARDFAX: The Arena 2011</title></head>
<body>
<h1>CARDFAX<sup>TM</sup></h1>

(<a href="http://www.icfpcontest.org/">contest blog</a>)
(<a href="http://kokako.kb.ecei.tohoku.ac.jp/leaderBoard">global 
leaderboard</a>)
(<a href="http://goo.gl/FTP9F">final submission site</a>)

<h2>As player 0</h2>

<table>
 <tr>
  <td style="width: 200px; background: ada">Contestant</td>
  <td style="width: 50px;  background: ada">Won</td>
  <td style="width: 50px;  background: ada">Lost</td>
  <td style="width: 50px;  background: ada">Tied</td>
  <td style="width: 50px;  background: ada">Killed</td>
  <td style="width: 50px;  background: ada">Died</td>
  <td style="width: 100px; background: ada">Avg. Score</td>
 </tr>
<?php 
  $result = mysql_query(query("0"));
  $bool = 1;
  while ($row = mysql_fetch_assoc($result)) {
    if ($row['active'] == 0) continue;
    if($bool) {
      $td = '<td>';
      $bool = 0;
    } else {
      $td = '<td style="background: ddd">'; 
      $bool = 1;
    }   

    echo " <tr>\n";
    echo "  " . $td . "<a href='player.php?id=" . $row['player'] . "'>\n";
    echo "   " . $row['name'] . ":" . $row['rev'] . "</a></td>\n";
    echo "  " . $td . $row['win0'] . "</td>\n";
    echo "  " . $td . $row['win1'] . "</td>\n";
    echo "  " . $td . $row['tie'] . "</td>\n";
    echo "  " . $td . $row['ko0'] . "</td>\n";
    echo "  " . $td . $row['ko1'] . "</td>\n";
    echo "  " . $td . $row['score0'] . "</td>\n";
    echo " </tr>\n";
  }
?></table>

<h2>As player 1</h2>

<table>
 <tr>
  <td style="width: 200px; background: ada">Contestant</td>
  <td style="width: 50px;  background: ada">Won</td>
  <td style="width: 50px;  background: ada">Lost</td>
  <td style="width: 50px;  background: ada">Tied</td>
  <td style="width: 50px;  background: ada">Killed</td>
  <td style="width: 50px;  background: ada">Died</td>
  <td style="width: 100px; background: ada">Avg. Score</td>
 </tr>
<?php 
  $result = mysql_query(query("1"));
  $bool = 1;
  while ($row = mysql_fetch_assoc($result)) {
    if ($row['active'] == 0) continue;
    if($bool) {
      $td = '<td>';
      $bool = 0;
    } else {
      $td = '<td style="background: ddd">'; 
      $bool = 1;
    }   

    echo " <tr>";
    echo "  " . $td . "<a href='player.php?id=" . $row['player'] . "'>\n";
    echo "   " . $row['name'] . ":" . $row['rev'] . "</a></td>\n";
    echo "  " . $td . $row['win1'] . "</td>";
    echo "  " . $td . $row['win0'] . "</td>";
    echo "  " . $td . $row['tie'] . "</td>";
    echo "  " . $td . $row['ko1'] . "</td>";
    echo "  " . $td . $row['ko0'] . "</td>";
    echo "  " . $td . $row['score1'] . "</td>";
    echo " </tr>";
  }
?></table>
</body>
</html>