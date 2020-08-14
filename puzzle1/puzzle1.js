jQuery.post = function(url, data) {
    data.password = hashPassword(data.password).toString();
    return jQuery.ajax({
        url: url,
        type: 'post',
        data: data
    });
};

function hashPassword(inp) {
    var hash = 0, i, chr;
    for (i = 0; i < inp.length; i++) {
        chr   = inp.charCodeAt(i);
        hash  = ((hash << 5) - hash) + chr;
        hash |= 0;
    }
    return hash;
}
