
<?php
$conn = new mysqli("localhost","root","","quizy");
$user_nick = mysqli_real_escape_string($conn, $_POST["name"]);
$user_email = mysqli_real_escape_string($conn, $_POST["email"]);
$user_password = mysqli_real_escape_string($conn, $_POST["password"]);
$user_password_hash = password_hash($user_password, PASSWORD_DEFAULT);
mysqli_query($conn, "INSERT INTO users (user_nick, user_email, user_passwordhash) VALUES ('$user_nick', '$user_email', '$user_password_hash')")

   echo "Rejestracja przebiegła poprawnie";

 ?>
