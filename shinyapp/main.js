$(function() {

  var bh = new Bloodhound({
    datumTokenizer: Bloodhound.tokenizers.whitespace,
    queryTokenizer: Bloodhound.tokenizers.whitespace
  });

  function search(q, sync) {
    var components = q.split(' ');
    var initial = components.slice(0, -1)
    var last = components[components.length - 1];
    bh.search(last, function(suggestions) {
      sync.call(null, suggestions.map(function(suggestion) {
        return initial.concat(suggestion).join(' ');
      }));
    });
  }

  Shiny.addCustomMessageHandler("myCallbackHandler", function(value) {
    console.log(value);
    bh.add(value);
    $('#text').trigger('change');
  });

  $('#text').typeahead({
    hint: true,
    // highlight: true,
    minLength: 1
  },
  {
    name: 'states',
    source: search
  });

});