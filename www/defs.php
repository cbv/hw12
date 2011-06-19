<?php

mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
mysql_select_db("hw12");

# Debug-mode wrapper for mysql_query()
function my_query($query) {
  if(isset($_REQUEST['debug'])) echo "\n" .$query . "\n";
  return mysql_query($query);
}

# Make a function from ids to player identifiers
# It is bad behavior to do this in slow PHP land instead of fast MySQL land,
# but screw you for pointing it out.
$result = my_query("select * from contestants");
while($row = mysql_fetch_assoc($result)) {
  $global_players[$row['id']][0] = $row['name'];
  $global_players[$row['id']][1] = $row['rev'];
  $global_players[$row['id']][2] = $row['name'] . ":" . $row['rev'];
  $global_players[$row['id']][3] = $row['active'];}

function player($id) {
  global $global_players;
  return $global_players[$id][2]; 
}

function active($id) {
  global $global_players;
  if($global_players[$id][3] == "1")
    return 1;
  else 
    return 0;
}

# Get scores with an optional "where" clause argument
function get_scores($where = "") {
  # Definitions
  $tie = "(dead0 + zomb0 = dead1 + zomb1)";
  $win0 = "(dead0 + zomb0 < dead1 + zomb1)";
  $win1 = "(dead0 + zomb0 > dead1 + zomb1)";
  $ko0 = "(rounds < 200000 && " . $win0 . ")";
  $ko1 = "(rounds < 200000 && " . $win1 . ")";
  $score0 = "(2 * " . $win0 . " + 4 * " . $ko0 . " + " . $tie . ") as s0"; 
  $score1 = "(2 * " . $win1 . " + 4 * " . $ko1 . " + " . $tie . ") as s1"; 

  # Query the database
  $query = "select min(id), player0, player1, count(id), avg(rounds), ";
  $query .= "avg(dead0), avg(dead1), avg(zomb0), avg(zomb1), ";
  $query .= "avg(vit0), avg(vit1), ";
  $query .= "avg" . $score0 . ", avg" . $score1 . ", ";
  $query .= "avg" . $win0 . ", avg" . $win1 . ", ";
  $query .= "avg" . $tie . ", avg" . $ko0 . ", avg" . $ko1 . " ";
  $query .= "from matches " . $where . " group by player0, player1";
  $result = my_query($query);

  # Populate the return array

  $ret = array();
  while($row = mysql_fetch_row($result)) {
    $id = $row[0];
    $p0 = intval($row[1]);
    $p1 = intval($row[2]);
    $dead0 = intval($row[5]);
    $dead1 = intval($row[6]);
    $zomb0 = intval($row[7]);
    $zomb1 = intval($row[8]);

    $data = array();
    $data['id'] = $id;
    $data['p0'] = $p0;
    $data['p1'] = $p1;
    $data['dups'] = $row[3];
    $data['rounds'] = $row[4];
    $data['dead0'] = $dead0;
    $data['dead1'] = $dead1;
    $data['zomb0'] = $zomb0;
    $data['zomb1'] = $zomb1;
    $data['live0'] = 256 - $dead0 - $zomb0;
    $data['live1'] = 256 - $dead1 - $zomb1;
    $data['vit0'] = intval($row[9]);
    $data['vit1'] = intval($row[10]);    
    $data['score0'] = floatval($row[11]);
    $data['score1'] = floatval($row[12]);
    $data['win0'] = floatval($row[13]);
    $data['win1'] = floatval($row[14]);
    $data['tie'] = floatval($row[15]);
    $data['ko0'] = floatval($row[16]);
    $data['ko1'] = floatval($row[17]);

    # if(!$isset(ret[$p0])) $ret[$p0] = array();
    $ret[$p0][$p1] = $data;
  }

  return $ret;
}


