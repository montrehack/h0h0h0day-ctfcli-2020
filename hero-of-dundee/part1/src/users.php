<?php 
  if(!isset($_COOKIE['debug'])) {
    setcookie('debug', 'false', time() + (86400 * 30), "/");
  } 
?>

<!DOCTYPE html> 
<html>
<head>
		<title>Chaos wizard's disciples</title>
		<meta charset="utf-8" />
		<meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no" />
		<link rel="stylesheet" href="assets/css/main.css" />
		<!--[if lte IE 9]><link rel="stylesheet" href="assets/css/ie9.css" /><![endif]-->
		<noscript><link rel="stylesheet" href="assets/css/noscript.css" /></noscript>
	</head>

<body>
  <!-- BG -->
  <div id="bg"></div>


  <!-- Wrapper -->
  <div id="wrapper">
    <h2>Disciples</h2>
    <form action="users.php" method="GET">
      <div>
        <input id="search" name="search" type="text" placeholder="Search by name">
        <input id="submit" type="submit" value="Search">
      </div>
    </form>
    <?php fetch_users(''); ?>
  </div>
</body>

<?php
  function fetch_users($username){
    $db = new SQLite3('/var/www/db/christmas-chase', SQLITE3_OPEN_CREATE | SQLITE3_OPEN_READWRITE);

    // Create a table.
    $db->query(
    'CREATE TABLE IF NOT EXISTS "users" (
        "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        "name" VARCHAR,
        "mysecretpwd" VARCHAR
      )'
    );

    $request = 'SELECT name FROM users WHERE name LIKE "' . $_GET['search'] . '%" AND name Not Like "%a%"';

    if($_COOKIE['debug'] == 'true')
    {
      echo '<h4 class="flag">FLAG-{Co0ki3$Ftw}</h4>';
    }

    $result = $db->query($request);

    echo '<p>Todo : Fix bug 215 : Some disciples seems to be filtered out</p>';
    echo '<table><tr><th>User name</th></tr>';

    while ($row = $result->fetchArray(SQLITE3_ASSOC)) 
    {
      echo '<tr>';
      foreach($row as $key=>$value) {
        echo '<td>' . $value . '</td>';
      }
      echo '</tr>';
    }

    '</table>';

    $db->close();
  }
  ?>
</html>