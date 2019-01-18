function setElementsEnabledByClassName(name, enabled) {
    let elements = document.getElementsByClassName(name + "_control");
    for(var i = 0; i < elements.length; i++) {
        if(elements[i].tagName.toLowerCase() == "input") {
            elements[i].disabled = !enabled;
        } else {
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
        disable("once");
        disable("repeated");
        disable("next_deadline");
    } else if(document.getElementById("once_radio").checked) {
        enable("once");
        disable("repeated");
        updateNextDeadlineControlBasedOnOnceHasDeadline();
    } else {
        disable("once");
        enable("repeated");
        updateRepeated();
        enable("next_deadline");
    }

}
