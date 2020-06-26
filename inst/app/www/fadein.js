function unfade(element) {
    var op = 0.5;  // initial opacity
    element.style.display = 'block';
    var timer = setInterval(function () {
        if (op >= 1){
            clearInterval(timer);
        }
        element.style.opacity = op;
        element.style.filter = 'alpha(opacity=' + op * 100 + ")";
        op += op * 0.1;
    }, 20);
}
unfade(document.getElementById('num_contain_2'))
unfade(document.getElementById('mod_contain_2'))
document.getElementById('mod_contain_2').style.pointerEvents = "auto";
document.getElementById('mod_contain_2').style.cursor = "default"; /* form cursor */
document.getElementById('mod2').style.cursor = "default"; /* div cursor */
document.getElementById('arrow1').classList.remove('no1');
document.getElementById('arrow1').classList.add('no2');

