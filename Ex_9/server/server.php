<!DOCTYPE html>

<html>
<head>
  <title>~ Play The Game ~</title>
  <?php
  function randomStr($round, $prevLen) {
    $chars = "abcdefghijklmnopqrstuvwxyz";
    $length = $prevLen + rand(5, 10);
    $ret = '';
    for ($i = 0; $i < $length; $i++) {
      $ret .= $chars[rand(0, $round)];
    }
    return $ret;
  }
  function lps($s) {
    $A = str_split($s);
    $n = count($A);
    $old = array_fill(0, $n + 1, 0);
    $curr = array_fill(0, $n , 1);
    $new = [];
    for($d = 1; $d < $n; $d++){
      for($i = 0; $i < $n-$d; $i++){
        $j = $i + $d;
        if ($A[$i] == $A[$j]) {
          $new[$i] = $old[$i+1] + 2;
        } else {
          $new[$i] = max($curr[$i], $curr[$i+1]);
        }
      }
      $old = $curr;
      $curr = $new;
    }

    return ($n-$curr[0]);
  }
  ?>
</head>
<body>
  <h1>
    PLAY THE GAME !
  </h1>
  <p>
    Longest Palindrome Subsequence
  </p>
  <p>
    Find the *least* possible number of letters that, if removed from the given
    string, what remains is a *palindrome*.
  </p>
  <?php
  session_start();
  if (!isset($_SESSION["round"])){
    echo "AA SESSION NOT SET";
    $_SESSION["round"] = 1;
    $_SESSION["question"] = randomStr(1,0);
  }
  ?>
  <p>
    Question <?php echo $_SESSION["round"] . ": length " . strlen($_SESSION["question"]); ?>
  </p>
  <p id="question"><?php echo $_SESSION["question"]; ?></p>
  <?php
  if ($_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST["answer"])) {
    if (lps($_SESSION["question"]) == $_POST["answer"]) {
      ?>
      <p class="right">
        Right! :-)
      </p>
      <?php
      if ($_SESSION["round"] < 10) {
        $_SESSION["question"] = randomStr($_SESSION["round"], strlen($_SESSION["question"]));
        $_SESSION["round"] += 1;
      } else {
        $_SESSION["question"] = randomStr(1, 0);
        $_SESSION["round"] = 1;
        ?>
        <p class="congratulations">
          Congratulation! Continue to try again.
        </p>
        <?php
      }
    } else {
      ?>
      <p class="wrong">
        Wrong! :-(
      </p>
      <?php
    }
    ?>
    <form method="post">
      <input type="submit" name="again" id="again" value="Continue!">
    </form>
    <?php
  } else {
    ?>
    <p>
      What is the least number of characters you need to remove?
    </p>
    <form method="post">
      <input type="text" id="answer" name="answer">
      <input type="submit" name="submit" id="submit" value="Submit!">
    </form>
    <?php
  }
  ?>
</p>
</body>
</html>
