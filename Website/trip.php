<html>
<body>
<?php

$servername = "mydb.itap.purdue.edu";
$username = "g1114008";
$password = "Group8!";
$dbname = "g1114008";

$conn = mysqli_connect($servername, $username, $password, $dbname);

if(!$conn) {
    die("<br>Connection failed:" . mysqli_connect_error());
}
echo "<br>Connected successfully";

 $name=$email=$password=$tripID="";
 $startLat=$startLong=$endLat=$endLong="";
 $FoodPreference=$HotelPreference="";
 if ($_SERVER["REQUEST_METHOD"] == "POST") {
     $tripID = $_POST["tripID"];
     $name = $_POST["name"];
     $email = $_POST["email"];
     $password = $_POST["password"];
     
     $startLat = $_POST["startLat"];
     $startLong = $_POST["startLong"];
     $endLat = $_POST["endLat"];
     $endLong = $_POST["endLong"];
     
     $FoodPreference = $_POST["FoodPreference"];
     $HotelPreference = $_POST["HotelPreference"];
 }
 $sql = "INSERT INTO Customers (Email, Password, Name) VALUES('$email', '$password', '$name')";
 $result = mysqli_query($conn, $sql);
 $sql = "INSERT INTO Trips (TripID, Email, HotelPreference, FoodPreference, StartLat, StartLong, EndLat, EndLong) VALUES('$tripID', '$email', '$HotelPreference', '$FoodPreference', '$startLat', '$startLong', '$endLat', '$endLong')";
 $result = mysqli_query($conn, $sql);
 mysqli_close($conn);
?>
</body>
</html>

