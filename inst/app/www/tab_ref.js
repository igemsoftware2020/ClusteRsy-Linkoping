const customHref = function(link){
    const links = document.getElementsByTagName("a");
    Object.entries(links).forEach( (elem, i) => {
        if(elem[1].getAttribute("data-value") === link){
            elem[1].click()
        }
    });
}
