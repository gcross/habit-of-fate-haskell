function setElementsEnabledByClassName(class_name, enabled) {
    let elements = document.getElementsByClassName("once_control");
    for(var i = 0; i < elements.length; i++) {
        if(elements[i].tagName.toLowerCase() == "input") {
            elements[i].disabled = !enabled;
        } else {
            if(!enabled) {
                elements[i].classList.add("disabled");
            } else {
                elements[i].classList.remove("disabled");
            }
        }
    }
}

function setOnceEnabled(enabled) { setElementsEnabledByClassName("once_control", enabled); }

function clickedIndefinite() {
    setOnceEnabled(false);
}

function clickedOnce() {
    setOnceEnabled(true);
}

function clickedRepeated() {
    setOnceEnabled(false);
}
