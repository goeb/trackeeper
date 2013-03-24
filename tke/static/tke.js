
function changeWrapping(nameOfTextArea){
    var textarea = document.getElementsByName(nameOfTextArea)[0];
    if(textarea.wrap != "off") textarea.wrap = "off";
    else textarea.wrap = "hard";
}

