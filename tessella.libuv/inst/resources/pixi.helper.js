
var repdraw;

function initialize_circles() {
    var canv = document.getElementById("mycanvas");
    var cont = canv.getContext("2d");
    cont.clearRect(0, 0, canv.width, canv.height);
    cont.strokeRect(0, 0, canv.width, canv.height);
    draw = function() {
	x = Math.random() * canv.width;
	y = Math.random() * canv.height;
	cont.beginPath();
	cont.arc(x, y, Math.random() * 10,
		 0, Math.PI * 2, true);
	cont.closePath();
	cont.strokeStyle = "#ff0000";
	cont.fillStyle = "#330000";
	cont.fill();
	cont.stroke();
    }
    if (repdraw) clearInterval(repdraw);
    repdraw = setInterval(draw, 200);
}


//Create the renderer
var renderer =
    new PIXI.CanvasRenderer(
    // PIXI.autoDetectRenderer(
	400, 400,
	{ antialias: false, transparent: true, resolution: 1 }
    );

//Create a container object called the `stage`
var stage = new PIXI.Container();

var stages = new Object();

function initPIXI()
{
    //Add the canvas to the HTML document
    document.body.appendChild(renderer.view);
    // set a border?
    renderer.view.style.border = "1px solid red";
    // misc stuff
    renderer.autoResize = true; // for any future resize
    //Tell the `renderer` to `render` the `stage`
    renderer.render(stage);
}

function initPage()
{
    initPIXI();
    stages[0] = stage; // for multiple layers later
}


function myrect()
{
    var rectangle = new PIXI.Graphics();
    rectangle.lineStyle(4, 0xFF3300, 1);
    rectangle.beginFill(0x66CCFF);
    rectangle.drawRect(170, 170, 64, 64);
    rectangle.endFill();
    stage.addChild(rectangle);
}

// basic primitives

gprim = {
    // c refers to the 'stage' (container)
    clear : function(c, fill) {
	c.removeChildren();
	if (fill) {
	    // FIXME: rendered should not be global, can be multiple
	    renderer.backgroundColor = fill;
	}
    }, 

    circle : function(c, x, y, r, stroke, fill, salpha, falpha)
    {
	var circle = new PIXI.Graphics();
	circle.lineStyle(1, stroke, salpha);
	if (falpha > 0) circle.beginFill(fill, falpha);
	circle.drawCircle(x, y, r);
	if (falpha > 0) circle.endFill();
	c.addChild(circle);
    },

    line : function(c, x1, y1, x2, y2, stroke, salpha)
    {
	var line = new PIXI.Graphics();
	line.lineStyle(1, stroke, salpha);
	line.moveTo(x1, y1);
	line.lineTo(x2, y2);
	c.addChild(line);
    },

    polygon : function(c, x, y, stroke, fill, salpha, falpha) {
	// TODO
	1;
    },

    rectangle1 : function(c, x0, y0, x1, y1, stroke, fill, salpha, falpha)
    {
	var rectangle = new PIXI.Graphics();
	rectangle.lineStyle(1, stroke, salpha);
	if (falpha > 0) rectangle.beginFill(fill, falpha);
	rectangle.drawRect(x0, y0, x1 - x0, y1 - y0);
	if (falpha > 0) rectangle.endFill();
	c.addChild(rectangle);
    },

    textdims1 : function(str, cex, rot) 
    {
	var s = new PIXI.Text(str, {font: "12px sans-serif", fill: "black", align : "center"});
	ws.send("c(" + s.width + "," + s.height + ")");
    },

    text1 : function(c, x, y, str, cex, rot) 
    {
	var s = new PIXI.Text(str, {font: "12px sans-serif", fill: "black", align : "center"});
	s.scale.set(cex, cex);
	s.rotation = rot / 180 * Math.PI;
	s.position.set(x, y);
	// console.log(s.width);
	// console.log(s.scale);
	c.addChild(s);

	// // font, cex, ps, col, gamma
	// if (col) {
	//     c.translate(x, y);
	//     if (rot != 0) c.rotate(-rot * 0.01745329); /* =pi/180 (rot : degrees -> radian) */
	//     c.fillStyle = col;
	//     c.textAlign = "start";
	//     c.textBaseline = "bottom";
	//     var fontwt = "normal";
	//     if (fontface == 2) fontwt = "bold";
	//     else if (fontface == 3) fontwt = "italic";
	//     else if (fontface == 4) fontwt = "bold italic";
	//     // c.font =  "italic 12px "Unknown Font", sans-serif"
	//     var ofont = c.font;
	//     c.font = fontwt + " " + Math.round(cex*ps) + "px " + fontfamily;
	//     c.fillText(str, -hadj * c.measureText(str).width, 0);
	//     c.font = ofont;
	//     c.setTransform(1, 0, 0, 1, 0, 0); // reset transformation
	// }
    }

    // clip : function(c, left, right, bottom, top) 
    // {
    // 	c.restore();
    // 	c.save();
    // 	c.beginPath();
    // 	c.moveTo(left, bottom);
    // 	c.lineTo(right, bottom);
    // 	c.lineTo(right, top);
    // 	c.lineTo(left, top);
    // 	c.closePath();
    // 	c.clip();
    // }

}


// Define API for R to talk to PIXI

// For starters, keep colors etc non-vectorized, and specified
// separately as theme-like settings. That is, parameters should be
// specified in separate calls. ALso, coordinates in calls made from R
// should be integers (i.e., conversion should be done in R).

targets = [ stage ];

var theme = {
    target : 0, // stage to target. May have multiple for layered drawing.
    stroke : 0x000000,
    fill : null,
    salpha : 1, // stroke alpha
    falpha : 0  // fill alpha
};

function setPar(s, val)
{
    theme[s] = val;
}

// note: there's room to optimize, e.g., by cloning rather than
// creating new PIXI.Graphics() objects for each point when parameters
// remain unchanged. Do these later.

function points(x, y)
{
    var i, n = x.length;
    with(gprim)
    {
	for (i = 0; i < n; i++)
	    circle(stages[theme['target']], x[i], y[i], 5,
		   theme['stroke'], theme['fill'],
		   theme['salpha'], theme['falpha']);
    }
}


function lines(x, y)
{
    var i, n = x.length;
    with(gprim)
    {
	for (i = 1; i < n; i++)
	    line(stages[theme['target']],
		 x[i-1], y[i-1], x[i], y[i],
		 theme['stroke'], theme['salpha']);
    }
}

function segments(x0, y0, x1, y1)
{
    var i, n = x0.length;
    with(gprim)
    {
	for (i = 0; i < n; i++)
	    line(stages[theme['target']],
		 x0[i], y0[i], x1[i], y1[i],
		 theme['stroke'], theme['salpha']);
    }
}

function rect(x0, y0, x1, y1)
{
    var i, n = x0.length;
    with(gprim)
    {
	for (i = 0; i < n; i++)
	    rectangle1(stages[theme['target']],
		       x0[i], y0[i], x1[i], y1[i],
		       theme['stroke'], theme['fill'],
		       theme['salpha'], theme['falpha']);
    }
}

function text(x, y, str, cex, rot)
{
    // only scalar for now
    var i, n = x.length;
    if (!cex) cex = 1;
    if (!rot) rot = 0;
    with(gprim)
    {
	text1(stages[theme['target']], x, y, str, cex, -rot);
    }
}

function textdims(str, cex, rot)
{
    if (!cex) cex = 1;
    if (!rot) rot = 0;
    with(gprim)
    {
	textdims1(str, cex, -rot);
    }
}

function newpage()
{
    with(gprim)
    {
	clear(stages[theme['target']]);
    }
}

function update()
{
    renderer.render(stages[theme['target']]);
}


