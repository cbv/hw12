<?php
  require_once('defs.php');

  foreach($global_players as $id => $vals) {
    $player[$id]['count']  = 0;
    $player[$id]['name']   = $vals[2];
    $player[$id]['won']    = 0.0;
    $player[$id]['lost']   = 0.0;
    $player[$id]['tied']   = 0.0;
    $player[$id]['killed'] = 0.0;
    $player[$id]['died']   = 0.0;
    $player[$id]['score']  = 0.0;
  }

  foreach(get_scores() as $p0 => $p0match) {
    foreach($p0match as $p1 => $row) {
      if(active($p1)) {
        $player[$p0]['count']  += 1;
        $player[$p0]['won']    += $row['win0'];
        $player[$p0]['lost']   += $row['win1'];
        $player[$p0]['tied']   += $row['tie'];
        $player[$p0]['killed'] += $row['ko0'];
        $player[$p0]['died']   += $row['ko1'];
        $player[$p0]['score']  += $row['score0'];
      }
       
      if(active($p0)) {
        $player[$p1]['count']  += 1;
        $player[$p1]['won']    += $row['win1'];
        $player[$p1]['lost']   += $row['win0'];
        $player[$p1]['tied']   += $row['tie'];
        $player[$p1]['killed'] += $row['ko1'];
        $player[$p1]['died']   += $row['ko0'];
        $player[$p1]['score']  += $row['score1'];       
      }
    }
  }

  foreach($player as $id => $vals) {
    if($player[$id]['count'] > 0) {
      $player[$id]['score'] = $vals['score'] / $vals['count'];
      $score[$id] = $vals['score'] / $vals['count'];
    } else {
      $score[$id] = 0.0;
    }
  }
  
  arsort($score);
?>

<html>
 <head><title>CARDFAX: The Arena 2011</title></head>
<body>
<h1>CARDFAX<sup>TM</sup></h1>

(<a href="http://www.icfpcontest.org/">contest blog</a>)
(<a href="http://kokako.kb.ecei.tohoku.ac.jp/leaderBoard">global 
leaderboard</a> - <a href="http://kokako.kb.ecei.tohoku.ac.jp/">sign in</a>)
(<a href="oldindex.php">old leaderboard</a>)
(<a href="http://goo.gl/FTP9F">final submission site</a>)

<table>
 <tr>
  <td style="width: 200px; background: ada; font-weight: bold">Contestant</td>
  <td style="width: 50px;  background: ada; font-weight: bold">Played</td>
  <td style="width: 50px;  background: ada; font-weight: bold">Won</td>
  <td style="width: 50px;  background: ada; font-weight: bold">Lost</td>
  <td style="width: 50px;  background: ada; font-weight: bold">Tied</td>
  <td style="width: 50px;  background: ada; font-weight: bold">Killed</td>
  <td style="width: 50px;  background: ada; font-weight: bold">Died</td>
  <td style="width: 100px; background: ada; font-weight: bold">Avg. Score</td>
 </tr>
<?php 
  $gray = 0;
  foreach($score as $id => $score) {
    if(!active($id)) continue;
    if($gray) {
      $td = '<td style="background: ddd">'; 
      $gray = 0;
    } else {
      $td = '<td>';
      $gray = 1;
    }   
 
    $nwon = $player[$id]['won'];
    $nlost = $player[$id]['lost'];
    $ntied = $player[$id]['tied'];

    echo " <tr>\n";
    echo "  " . $td . "<a style='color: #000' href='player.php?id="
         . $id . "'>\n";
    echo "   " . $player[$id]['name'] . "</a></td>\n";
    echo "  " . $td . ($nwon + $nlost + $ntied) . "</td>\n";
    echo "  " . $td . $nwon . "</td>\n";
    echo "  " . $td . $nlost . "</td>\n";
    echo "  " . $td . $ntied . "</td>\n";
    echo "  " . $td . $player[$id]['killed'] . "</td>\n";
    echo "  " . $td . $player[$id]['died'] . "</td>\n";
    echo "  " . $td . number_format($score, 4) . "</td>\n";
    echo " </tr>\n";
  }
?></table>
</body>
</html>