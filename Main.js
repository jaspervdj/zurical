"use strict";

exports.innerText = function(element) {
    return function() {
        return element.innerText;
    };
};

exports.renderDateTime = function(date) {
    return function() {
        return date.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'});
    };
};
