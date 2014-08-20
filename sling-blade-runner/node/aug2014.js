// Sling Blade Runner
// August 2014 attempt using Node.js

var LBL = require('line-by-line');

solve('../MOVIES.TEST');

function solve(file) {
    xref = make_xref(file);
}

function make_xref(file) {
    var fin = new LBL(file);
    var line_no = 0;

    var titles = {};

    fin.on('line',
	   function(title) {
	       var index = line_no;
	       titles[index] = title;

	       line_no++;
	   });

    fin.on('end',
	   function() {
	       console.log(titles);
	       console.log(line_no + " titles read");
	   }); 
}
