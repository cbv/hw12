<?php
  mysql_connect("localhost", "root", "R_E_D_A_C_T_E_D");
  mysql_select_db("hw12");
  $query = "select name, rev from contestants where active = 1";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    echo $row['name'] . ":" . $row['rev'] . "\n";
  }
