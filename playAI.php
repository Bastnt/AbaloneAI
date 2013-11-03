<?php
	set_time_limit(30);
	usleep(500000);

	if(isset($_POST["exec"])) {
		exec($_POST["exec"]." ".$_POST["player"]." ".$_POST["board"], $ans);
		foreach($ans as $a) {
			echo $a."\n";
		}
	}
?>