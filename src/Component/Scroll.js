exports.getScrollTop = function () {
    return (window.pageYOffset || document.documentElement.scrollTop);
}

exports.getScrollLeft = function () {
    return (window.pageXOffset || document.documentElement.scrollLeft);
}
