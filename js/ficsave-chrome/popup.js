document.addEventListener('DOMContentLoaded', function() {
	var downloadButton = document.getElementById('download');
	var formatSelect = document.getElementById('format');
	downloadButton.addEventListener('click', function() {
		chrome.tabs.getSelected(null, function(tab) {
			chrome.tabs.create({'url': "http://ficsave.com/?url="+tab.url+"&format="+formatSelect.value+"&download=yes"});
		})
	})
})
