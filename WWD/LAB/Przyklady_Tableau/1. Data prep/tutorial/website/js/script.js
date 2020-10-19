
function OnScroll () {
	var head = document.getElementById("hdhd");
	var hiddenspacer = document.getElementById("hiddenspacer");
	var body = document.getElementsByTagName("BODY")[0];
	var y = body.scrollTop;
	if (y > 40) {
		head.classList = "row fixedheader";
		hiddenspacer.style.display = "inline";
	} else {
		head.classList = "row";
		hiddenspacer.style.display = "none";
	}
}
function showHide() {
	if (document.getElementById("challenge").style.display == "block") {
		document.getElementById("challenge").style.display = "none";
		document.getElementById("solution").style.display = "block";
		document.getElementById("switch").value = "View Challenge";
	} else {
		document.getElementById("challenge").style.display = "block";
		document.getElementById("solution").style.display = "none";
		document.getElementById("switch").value = "View Solution";
	}
}
