var makeBSS = function (el, options) {
    var $slideshows = document.querySelectorAll(el), // a collection of all of the slideshow
        $slideshow = {},
        Slideshow = {
            init: function (el, options) {
                this.counter = 0; // to keep track of current slide
                this.el = el; // current slideshow container    
                this.$items = el.querySelectorAll('figure'); // a collection of all of the slides, caching for performance
                this.numItems = this.$items.length; // total number of slides
                options = options || {}; // if options object not passed in, then set to empty object 
                options.auto = options.auto || false; // if options.auto object not passed in, then set to false
                this.opts = {
                    auto: (typeof options.auto === "undefined") ? false : options.auto,
                    speed: (typeof options.auto.speed === "undefined") ? 1000 : options.auto.speed,
                    pauseOnHover: (typeof options.auto.pauseOnHover === "undefined") ? false : options.auto.pauseOnHover,
                };  
                this.$items[0].className += " bss-show";
				if (this.opts.auto) {
                    this.autoCycle(this.el, this.opts.speed, this.opts.pauseOnHover);
                }
            },
            showCurrent: function (i) {
                // increment or decrement this.counter depending on whether i === 1 or i === -1
                if (i > 0) {
                    this.counter = (this.counter + 1 === this.numItems) ? 0 : this.counter + 1;
                } else {
                    this.counter = (this.counter - 1 < 0) ? this.numItems - 1 : this.counter - 1;
                }
                // remove .show from whichever element currently has it 
                // http://stackoverflow.com/a/16053538/2006057
                [].forEach.call(this.$items, function (el) {
                   // el.classList.remove('bss-show');
				   el.className -= " bss-show";
                }); 
                // add .show to the one item that's supposed to have it
                //this.$items[this.counter].classList.add('bss-show');
				this.$items[this.counter].className += " bss-show";
            },

            autoCycle: function (el, speed, pauseOnHover) {
                var that = this,
                    interval = window.setInterval(function () {
                        that.showCurrent(1); // increment & show
                    }, speed);
     // IE8 DOES NOT SUPPORT addEventListener, attachEvent USED INSTEAD
                if (pauseOnHover) {
					if (el.addEventListener) {
						el.addEventListener('mouseover', function () {
							interval = clearInterval(interval);
						}, false);
					}
					else {
						el.attachEvent("onmouseover", function () {
							interval = clearInterval(interval);});
					}
//					el.addEventListener('mouseover', function () {
//                       interval = clearInterval(interval);
//                    }, false);
  // IE8 DOES NOT SUPPORT addEventListener, attachEvent USED INSTEAD
					if (el.addEventListener) {
						el.addEventListener('mouseout', function () {
							interval = window.setInterval(function () {
								that.showCurrent(1); // increment & show
							}, speed);
						}, false);
					}
					else {
						el.attachEvent("onmouseout",  function () {
							interval = window.setInterval(function () {
								that.showCurrent(1); // increment & show
							}, speed);
						});
					}
//                    el.addEventListener('mouseout', function () {
//                        interval = window.setInterval(function () {
//                            that.showCurrent(1); // increment & show
//                        }, speed);
//                    }, false);
                }                
            },            
        }; // end Slideshow object 
  
    // make instances of Slideshow as needed
// IE8 DOES NOT SUPPORT forEach FUNCTION, FUNCTION MUST BE DECLARED
	if (typeof Array.prototype.forEach != 'function') {
		Array.prototype.forEach = function(callback){
		  for (var i = 0; i < this.length; i++){
			callback.apply(this, [this[i], i, this]);
		  }
		};
	}
    [].forEach.call($slideshows, function (el) {
// IE8 DOES NOT SUPPORT Object.create, FUNCTION MUST BE CREATED		
		if (!Object.create) {
		Object.create = function(o) {
			if (typeof o !== 'object' && typeof o !== 'function') throw new TypeError('Object prototype may only be an Object: ' + o);
			else if (o === null) throw new Error("This browser's implementation of Object.create is a shim and doesn't support 'null' as the first argument.");
			function F() {}
			F.prototype = o;
			return new F();
		};}
        $slideshow = Object.create(Slideshow);
        $slideshow.init(el, options);
    });
};
var opts = {
    auto : {
        speed : 3000, 
        pauseOnHover : true
    }

};
window.onload = function (){
	makeBSS('.slideshow', opts);
}
