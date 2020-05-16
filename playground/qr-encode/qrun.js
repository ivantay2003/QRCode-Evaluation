var qr = require('qr-encode')


global.getQR = function (text){
    
    var dataURI = qr(text, {type: 6, size: 6, level: 'Q'})
    console.log ("success")
    
    var answer = document.getElementById('answer')
    console.log ("text : "+ text)
    
    var url = document.getElementById ("image")
    url.src = dataURI 
    document.body.appendChild(url)
    return dataURI
}


//If using in browsers:
