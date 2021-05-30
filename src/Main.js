"use strict";

exports.innerText = function(element) {
    return function() {
        return element.innerText;
    };
};
