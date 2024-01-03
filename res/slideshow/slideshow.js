// Slideshow 2.00 (c) 2023 Jan Schirrmacher
// Revision History 
// V1.00 JS 2022-10-22 Initial release
// V1.10 JS 2022-10-23 Added cinema mode, renamed image to picture
// V1.20 JS 2022-10-28 Added autoStop, more properties instead of methods, css improvements, fullscreen-support
// V1.30 JS 2022-10-28 Fullscreen control added
// V2.00 JS 2023-12-05 Redesign

// helper to enable % for negative numbers
function mod(n, m) {
  return ((n % m) + m) % m;
}

const DEFAULTCINEMAINTERVAL = 3000;
const MINCINEMAINTERVAL = 100;

// Needs an HTML doc with certain elements and proper class names.
class Slideshow {

  // _container is the element (block) which containes the slideshow.
  constructor(container, imageList = null) {
    this._container = container;
    this._images = this.queryAll('img');
    this._bullets = this.queryAll('.control-bar svg');
    this._imageIndex = -1;
    this._cinemaInterval = DEFAULTCINEMAINTERVAL;
    this._cinemaTimerId = null;
    this._isCinema = false;
    this.autoStop = true;

    for (let i=0; i<this.imageCount; i++) {
      this._bullets[i].addEventListener('click', ()=>{
        this.imageIndex = i;
      })
    }

    this.query('.button-prev').addEventListener('click', ()=>{
      this.imageIndex -= 1;
    })

    this.query('.button-next').addEventListener('click', ()=>{
      this.imageIndex += 1;
    })

    this.query('.button-cinema').addEventListener('click', ()=>{
      if (this.isCinema)
        this.stop();
      else
        this.start(true);
    })

    this.query('.button-fullscreen').addEventListener('click', ()=>{
      this.fullscreen = ! this.fullscreen;
    })

    this._container.addEventListener('fullscreenchange', () => {
      if (this.fullscreen)
        this.setUse('.button-fullscreen', '#svg-shrink');
      else
        this.setUse('.button-fullscreen', '#svg-expand');
    });

    this.imageIndex = 0;

  }

  query(selector) {
    return document.querySelector(`#${this._container.id} ${selector}`)
  }

  queryAll(selector) {
    return document.querySelectorAll(`#${this._container.id} ${selector}`)
  }

  setUse(svgClass, value) {
    this.query(svgClass).children[0].setAttribute('href', value);
  }

  get imageCount() {
    return this._images.length;
  }

  get imageIndex() {
    return this._imageIndex;
  }

  set imageIndex(value) {
    value = mod(value, this.imageCount);
    if (value!=this._imageIndex) {     
      if (this._imageIndex != -1) {
        this._images[this._imageIndex].classList.replace('image-visible', 'image-hidden');
        this._bullets[this._imageIndex].children[0].setAttribute('href', '#svg-circle');
      }              
      this._imageIndex = value;
      this._images[this._imageIndex].classList.replace('image-hidden', 'image-visible');
      this._bullets[this._imageIndex].children[0].setAttribute('href', '#svg-selected');
      this.query('.title-bar').textContent = this._images[this._imageIndex].alt;
    }
  }

  get isCinema() {
    return this._cinemaTimerId != null;
  }

  get cinemaInterval() {
    return this._cinemaInterval;
  }

  set cinemaInterval(value) {
    if (value!=this._cinemaInterval) {
      if (value<MINCINEMAINTERVAL) value=MINCINEMAINTERVAL;
      this._cinemaInterval = value;
      if (this.isCinema) {
        this.stop();
        this.start();
      }
    }
  }

  start(immediate = false) {
    if (!this.isCinema) {
      this._cinemaTimerId = setInterval(() => {
        this.imageIndex += 1;
      }, this._cinemaInterval);
      this.setUse('.button-cinema', '#svg-frame-pause');
      if (immediate)
        this.imageIndex = 1;
    }
  }

  stop() {
    if (this.isCinema) {
      clearInterval(this._cinemaTimerId);
      this._cinemaTimerId = null;
      this.setUse('.button-cinema', '#svg-frame-start');
    }
  }

  speed(factor = 1.5) {
    this.cinemaInterval /= factor;
  }  

  get fullscreen() {
    return document.fullscreenElement != null;
  }

  set fullscreen(value) {
    if (value==this.fullscreen) return;
    if (value)
      this._container.requestFullscreen();
    else
      document.exitFullscreen();        
  }

}
