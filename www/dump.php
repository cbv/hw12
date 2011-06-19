player1,player2,dead0,dead1,zomb0,zomb1,live0,live1
<?php
  require_once('defs.php');

  foreach(get_scores() as $p0 => $matches) {
    foreach($matches as $p1 => $row) {
      echo player($row['p0']) . ",";
      echo player($row['p1']) . ",";
      echo $row['dead0'] . ",";
      echo $row['dead1'] . ",";
      echo $row['zomb0'] . ",";
      echo $row['zomb1'] . ",";
      echo $row['live0'] . ",";
      echo $row['live1'] . ",";
      echo $row['score0'] . ",";
      echo $row['score1'] . "\n";
    }
  } 