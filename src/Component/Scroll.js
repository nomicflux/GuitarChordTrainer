exports.getScrollTop = function () {
    return (window.pageYOffset || document.documentElement.scrollTop);
}

exports.getScrollLeft = function () {
    return (window.pageXOffset || document.documentElement.scrollLeft);
}

exports.getOffset = function ( el ) {
    return function() {
        var x = 0;
        var y = 0;
        /*
        while( el && !isNaN( el.offsetLeft ) && !isNaN( el.offsetTop ) ) {
            x += el.offsetLeft - el.scrollLeft;
            y += el.offsetTop - el.scrollTop;
            el = el.offsetParent;
        }
        */
        x = el.offsetLeft - (window.pageXOffset || document.documentElement.scrollLeft);
        y = el.offsetTop - (window.pageYOffset || document.documentElement.scrollTop);
        return { top: y, left: x };
    };
}
