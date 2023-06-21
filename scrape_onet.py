import base64
import requests

## define username and password 
username = "lse" 
password = "6463vhk"

class OnetWebService:
    def __init__(self, username, password):
        self._headers = {
            'User-Agent': 'python-OnetWebService/1.00 (bot)',
            'Authorization': 'Basic ' + base64.b64encode((username + ':' + password).encode()).decode(),
            'Accept': 'application/json'
        }
        self._url_root = 'https://services.onetcenter.org/ws/'

    def call(self, path, query=None):
        url = self._url_root + path
        response = requests.get(url, headers=self._headers, params=query)
        if response.status_code == 200:
            return response.json()
        else:
            return {
                'error': 'Call to ' + url + ' failed with error code ' + str(response.status_code),
                'response': response.text
            }


onet_ws = OnetWebService(username, password)


