"use strict";

export const innerText = function(element) {
    return function() {
        return element.innerText;
    };
};
