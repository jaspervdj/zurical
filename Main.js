"use strict";

export const innerText = function(element) {
    return function() {
        return element.innerText;
    };
};

export const renderDateTime = function(date) {
    return function() {
        return date.toLocaleTimeString([], {hour: '2-digit', minute:'2-digit'});
    };
};
