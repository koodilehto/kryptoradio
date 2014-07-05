// bitcoin progress bar by IS_A_CAT. This is proof of cnecept work and needs some serious tinkering/cleaning. 
// To use, stick thi javascript on your page. innerbtc will take up a percentage of it's parent div relative to the percentage of your goal raised. This is compatible with most css progress bars (anything using the div in div method).
var address = '1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5',
    total = 4776, // Your bitcoin goal
    currency = "EUR"; // anything from this list https://blockchain.info/ticker
var info, ticker;

function ajax1() {
    return $.ajax({
        type: 'GET',
        url: 'http://json2jsonp.com/?url=http://blockchain.info/ticker?q=&callback=?',
        dataType: 'jsonp',
        success: function (data) {
            ticker = data;
        }
    });
}

function ajax2() {
    return $.ajax({
        type: 'GET',
        url: 'http://json2jsonp.com/?url=http://blockchain.info/address/' + address + '?format=json&callback=?',
        dataType: 'jsonp',
        success: function (data) {
            info = data;
        }
    });
}
$.when(ajax1(), ajax2()).done(function () {
    begin();
});

function begin() {
    var stat = ticker[currency];
    var bal = info.final_balance;
    var raw = bal * stat.sell / 100000000;
    var human = stat.symbol + raw.toFixed(2); // human readable balance to 2dp with currency symbol 
    var percentage = raw / total * 100;
    var width = percentage > 100 ? 100 : percentage;
    $('#innerbtc').animate({
        width: width + "%"
    }, 2000);
    $('.ernbtc').html(human);
    $('.totalbtc').html(stat.symbol + total);
    $('.percentagebtc').html(percentage | 0);
}
