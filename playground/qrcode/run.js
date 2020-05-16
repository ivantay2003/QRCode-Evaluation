var QRCode = require('qrcode')
var canvas = document.getElementById('canvas')
 



function runQRCode (){
    
//    QRCode.toCanvas(canvas, 'sample text', function (error) {
//      if (error) console.error(error)
//      console.log('success!');
//    })
    
    console.log ("asa")
}

//function fishy (){
//    
//    
//    console.log ("this is fishy")
//}

global.fishy = function(text) {
    
    console.log ("this is fishy")
    
        QRCode.toCanvas(canvas, text, function (error) {
        if (error) console.error(error)
        console.log('success!');
        
        var answer = document.getElementById('answer')
        console.log ("text : "+ text)
        //answer.innerHTML = text
    })

}
