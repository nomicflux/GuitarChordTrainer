"use strict";

exports.setCookieImpl = function (name, value) {
    return function() {
        Cookies.set(name, value);
    };
}

exports.getCookieImpl = function (name) {
    return function() {
        var result = Cookies.getJSON(name);
        if (result)  {
            return result;
        } else {
            return null;
        };
    };
}

exports.removeCookieImpl = function(name) {
    return function() {
        Cookies.remove(name);
    }
}
