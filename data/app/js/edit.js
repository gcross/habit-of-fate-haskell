function setElementsEnabledByClassName(name, enabled) {
    console.log("SETTING " + name + " TO " + enabled);
    let elements = document.getElementsByClassName(name + "_control");
    console.log("ELEMENTS = " + elements);
    for(var i = 0; i < elements.length; i++) {
        console.log(elements[i]);
        if(elements[i].tagName.toLowerCase() == "input") {
            console.log("CASE A");
            elements[i].disabled = !enabled;
        } else {
            console.log("CASE B");
            if(enabled) {
                elements[i].classList.remove("disabled");
            } else {
                elements[i].classList.add("disabled");
            }
        }
    }
}

function enable(name) { setElementsEnabledByClassName(name, true); }
function disable(name) { setElementsEnabledByClassName(name, false); }

function updateRepeated() {
    if(document.getElementById("daily_radio").checked) {
        enable("daily");
        disable("weekly");
    } else {
        disable("daily");
        enable("weekly");
    }
}

function updateNextDeadlineControlBasedOnOnceHasDeadline() {
    if(document.getElementById("once_has_deadline").checked) {
        enable("next_deadline");
    } else {
        disable("next_deadline");
    }
}

function updateEnabled() {
    if(document.getElementById("indefinite_radio").checked) {
        console.log("CHOSEN INDEFINITE");
        disable("once");
        disable("repeated");
        disable("next_deadline");
    } else if(document.getElementById("once_radio").checked) {
        console.log("CHOSEN ONCE");
        enable("once");
        disable("repeated");
        updateNextDeadlineControlBasedOnOnceHasDeadline();
    } else {
        console.log("CHOSEN REPEATED");
        disable("once");
        enable("repeated");
        updateRepeated();
        enable("next_deadline");
    }

}
