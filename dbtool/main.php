<?php
// usage:
// $db = new database();                 creates a connection
// $db->create_table();                  creates the appropriate table (currencies)
//                                       should only run once.
// $db->create('EUR','USD',1.5,FALSE);   creates a row for the pair EUR,USD with the value of 1.5 
// $db->create('EUR','USD',1.5,TRUE);    creates a row for the pair EUR,USD as well as the pair USD,EUR
//                                       which will have the inverted value (i.e. 5 will turn to 0.2).
// $db->update('EUR','USD',3.0,FALSE);   updates the pair of EUR,USD to have the value of 3.0.
//                                       will only work on existing rows.
// $db->update('EUR','USD',3.0,TRUE);    updates the pair of EUR,USD to have the value of 3,
//                                       but also updates the pair of USD,EUR to have the value of 1/3.
// echo $db->value('EUR','USD');         echoes the value of the pair EUR,USD.
//
//                                       important note:
//                                       value will return a string error rather than a double
//                                       in case something is wrong with value!

// notes:
// the login.php file should be modified.
// the pair EUR,USD having the value 1.25 would imply that 1 EUR = 1.25 USD.
// currency codes should be in uppercase and comply with ISO 4217 standards.
// the database stores the value as a double.

class database extends mysqli {

    public function __construct() {
        include_once 'login.php';
        $this->mysqli = parent::__construct($hostname,$username,$password,$database);
    }

    // creates a new row.
    function create($currency1, $currency2, $value, $invert) {
        // if one of the two currency codes entered isn't a valid ISO 4217 code,
        // throws an exception.
        include_once 'validate.php';
        if (!(validate($currency1) && validate($currency2))) {
            $error = (string) 'Error: Some currency codes are not valid ISO 4217 currency code.\nTested: ' . $currency1 .', ' . $currency2;
            throw new Exception($error);
        }
        // ensures $value is a double
        if (is_double($value)) {
            if ($invert) {
                // also creates the inverted value to the database (i.e. in the case of EUR/USD, creates USD/EUR).
                $inverted_value = $this->invert($value);
                $this->create($currency2, $currency1, $inverted_value, FALSE);
            }   
            $query = "insert into currencies(value, currency1, currency2) values(?, ?, ?)";
            if ($stmt = parent::prepare($query)) {   
                $stmt->bind_param('dss',$value, $currency1, $currency2);
                $stmt->execute();
                $stmt->close();
            }
            else {
                $error = 'Problem with creating row: ' . $query . '.\n' . $this->mysqli->error;
                exit($error);
            }
        }
        else {
            $error = "Value $value inserted into function create is not a double!";
            exit($error);
        }
    }

    // used to update (EXISTING) rows.
    function update($currency1, $currency2, $value, $invert) {
        // ensures $value is a double
        if (is_double($value)) {
            if ($invert) {
                $inverted_value = $this->invert($value);
                $this->update($currency2, $currency1, $inverted_value, FALSE);
            }
       
            $query = "update currencies SET value = ? where currency1 = ? and currency2 = ?";
            if ($stmt = parent::prepare($query)) {   
                $stmt->bind_param('dss', $value, $currency1, $currency2);
                $stmt->execute();
                $stmt->close();
            }
            else {
                $error = 'Problem with updating row: ' . $query . '.\n' . $this->mysqli->error;
                exit($error);
            }
        }
        else {
            $error = "Value $value inserted into function update is not a double!";
            exit($error);
        }
    }

    function invert($number) {
        return (double) bcpow($number, '-1', 20);
    }

    // creates the table. should only run once.
    function create_table() {
    $query =
    "create table currencies (
    currency1	varchar(3),
    currency2	varchar(3),
    value		double,
    primary key	(currency1, currency2)
    )";

    if ($stmt = parent::prepare($query)) {   
        $stmt->execute();
        $stmt->close();
        }
    else {
            $error = 'Problem with creating table: ' . $query . '.\n' . $this->mysqli->error;
            exit($error);
    }
    }

    // returns the value of a currency pair. the returned value will be of type double.
    function value($currency1, $currency2) {
        $query = "SELECT value FROM currencies WHERE currency1 = ? AND currency2 = ?";
        if ($stmt = parent::prepare($query)) {   
            $stmt->bind_param('ss',$currency1, $currency2);
            $stmt->execute();
            $stmt->bind_result($value);
            $stmt->fetch();
            $stmt->close();
            return $value;
        }
        else {
            $error = (string) "Problem with finding value: " . $query . $this->mysqli->error;
            exit($error);
        }
    }
	
    function __destruct() {
        parent::close();
    }
}

?> 

