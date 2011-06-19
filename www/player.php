<?php
  require_once('defs.php');

  if(!isset($_REQUEST['id'])) die("No identifier given!");
  $id = $_REQUEST['id'];
  if(!isset($global_players[$id])) die("Not an identifier: " . $id);
  
  $cumcount = 0;
  $cumscore = 0.0;
  $cumactivecount = 0;
  $cumactivescore  = 0.0;
  $myname = player($id);
  $allscores = get_scores(" where player0 = " . $id . " || player1 = " . $id);
  foreach($allscores as $p0 => $p0match) {
    foreach($p0match as $p1 => $row) {
      $match = $row['id'];

      $player[$match]['img_util'] =
         "/graphs/" . $global_players[$p0][0] . "-" . $global_players[$p0][1]
         . "-" . $global_players[$p1][0] . "-" . $global_players[$p1][1] 
         . ".png";

      # Row, row, row your table
      if($id == $row['p0']) {
        # Store data for correctly sorting matches
        $scores[$row['score0']][$match] = $row['live0'] - $row['live1'];

        $player[$match]['score'] = $row['score0'];
        if($player[$match]['score'] > 4.0)
          $player[$match]['color'] = "00a";
        else if ($player[$match]['score'] > 1.5)
          $player[$match]['color'] = "0a0";
        else if ($player[$match]['score'] > 0.5)
          $player[$match]['color'] = "000";
        else
          $player[$match]['color'] = "f00";
        
        $player[$match]['redo'] = 
           "<a style='color: #" . $player[$match]['color']
           . "' href='delete.php?p0=" . $p0 . "&p1=" . $p1 . "'>";

        $player[$match]['p0'] = "<center><i>---- me ----</i></center>";
        $player[$match]['p1'] = "<a href='player.php?id=" . $p1 . "'"
                                . " style='color: #" . $player[$match]['color']
                                . "'>" . player($row['p1']) . "</a>";
        $player[$match]['mylive'] = $row['live0'];
        $player[$match]['theirlive'] = $row['live1'];
        $player[$match]['rounds'] = ceil($row['rounds'] / 2);
      } else { 
        # Store data for correctly sorting matches
        $scores[$row['score1']][$match] = $row['live1'] - $row['live0'];
        
        $player[$match]['score'] = $row['score1'];
        if($player[$match]['score'] > 4.0)
          $player[$match]['color'] = "00a";
        else if ($player[$match]['score'] > 1.5)
          $player[$match]['color'] = "0a0";
        else if ($player[$match]['score'] > 0.5)
          $player[$match]['color'] = "000";
        else
          $player[$match]['color'] = "f00";
        
        $player[$match]['redo'] = 
           "<a style='color: #" . $player[$match]['color']
           . "' href='delete.php?p0=" . $p0 . "&p1=" . $p1 . "'>";

        $player[$match]['p0'] = "<a href='player.php?id=" . $p0 . "'"
                                . " style='color: #" . $player[$match]['color']
                                . "'>" . player($row['p0']) . "</a>";
        $player[$match]['p1'] = "<center><i>---- me ----</i></center>";
        $player[$match]['mylive'] = $row['live1'];
        $player[$match]['theirlive'] = $row['live0'];
        $player[$match]['rounds'] = ceil($row['rounds'] / 2);
      }

      # Record cumulative score
      if($id == $row['p0']) {
        $cumcount += 1;
        $cumscore += $row['score0']; 
        if(active($p1)) {
          $cumactivecount += 1;
          $cumactivescore += $row['score0']; 
        }
      }
      if($id == $row['p1']) {
        $cumcount += 1;
        $cumscore += $row['score1']; 
        if(active($p0)) {
          $cumactivecount += 1;
          $cumactivescore += $row['score1']; 
        }
      }
    }
  }
?>

<html>
 <head><title><?php echo $myname ?></title></head>
<body>
<h1><?php echo $myname ?></h1>

<p>(<a href="/arena/">home</a>)
(<a href="oldplayer.php?id=<?php echo $id ?>">old page</a>)</p>

<span style="font-style: italic">
Average score 
(active opponents only):
<?php 
  if($cumactivecount == 0) 
    echo "NaN";
  else 
    echo number_format($cumactivescore / $cumactivecount, 4);
?><br/>
Average score 
(all registered opponents, past and present <strike>and future</strike>): 
<?php 
  if($cumcount == 0) 
    echo "NaN";
  else 
    echo number_format($cumscore / $cumcount, 4);
?>
</span>

<table>
 <tr>
  <td style="width: 200px; background: ada; font-weight: bold">Player 0</td>
  <td style="width: 200px; background: ada; font-weight: bold">Player 1</td>
  <td style="width: 70px; background: ada; font-weight: bold">My live</td>
  <td style="width: 70px; background: ada; font-weight: bold">Their live</td>
  <td style="width: 70px; background: ada; font-weight: bold">Rounds</td>
  <td style="width: 70px; background: ada; font-weight: bold">Score</td>
  <td style="width: 200px; background: ada; font-weight: bold">Tools</td>
 </tr>
<?php
  $gray = 0;
  ksort($scores);
  foreach($scores as $score => $inscore) {
    asort($inscore);
    foreach($inscore as $match => $rating) {
      $row = $player[$match];

      $fs = floatval($row['score']);
      if($fs > 4.0)       $color = "00a";
      else if ($fs > 1.5) $color = "0a0";
      else if ($fs > 0.5) $color = "000";
      else                $color = "f00";

      if($gray) {
        $bgcolor = "ddd";
        $gray = 0;
      } else {
        $bgcolor = "fff";
        $gray = 1;
      }         

      $td = '<td style="background: #' . $bgcolor 
            . '; color: #' . $color
            . '">'; 

      echo " <tr>\n";
      echo "  " . $td . $row['p0'] . "</td>\n";
      echo "  " . $td . $row['p1'] . "</td>\n";
      echo "  " . $td . $row['mylive'] . "</td>\n";
      echo "  " . $td . $row['theirlive'] . "</td>\n";
      echo "  " . $td . $row['rounds'] . "</td>\n";
      echo "  " . $td . $row['score'] . "</td>\n";
      echo "  " . $td 
         . "(<a style='color: #" . $row['color'] 
         . "' href='" . $row['img_util'] . "'>img</a>) "
         . "(" . $row['redo'] . "redo</a>)</td>\n";
      echo " </tr>\n";
    }
  }
?></table>
</body>
</html>