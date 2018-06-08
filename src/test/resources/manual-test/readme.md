JSON files for manual tests
---------------------------

* Launch a virtual machine with `vagrant up` in the root of the project
* Create a deposit, for details see `docs/api.html`
* Use the returned UUID to replace XXX in the following command.

    curl -i  -H 'Content-Type: text/plain'  --data-binary "@XXX.json" -X POST -u user001:user001 'http://test.dans.knaw.nl:20190/deposit/XXX/datasetmetadata'


Purpose of HTML files
---------------------

Examine cookies with developer tools of a browser without a fully dressed web client.