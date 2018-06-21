JSON files for manual tests
---------------------------

DatasetXmlSpec shows whether the `datasetmetadata-` files would produce valid DDM on submission.
The files with `datasetmetadata-from-ui-` in their name are derived from the mockserver of [easy-deposit-ui](https://github.com/DANS-KNAW/easy-deposit-ui).
`servlet\IntegrationSpec` specifies only some scenarios that could be performed by the UI.
Other tests show in the `servlets` package emulate just a single step.
All these steps can be tested manually with tools like `Postman` or `curl`.
One step involving json files is shown below.
Play with the content of the files to check whether responses make sense for the UI and user of the UI.

* Launch a virtual machine with `vagrant up` in the root of the project
* Create a deposit, for details see `docs/api.html`
* Use the returned UUID to replace XXX in the following command.

    curl -i  -H 'Content-Type: text/plain'  --data-binary "@XXX.json" -X POST -u user001:user001 'http://test.dans.knaw.nl:20190/deposit/XXX/datasetmetadata'


Purpose of HTML files
---------------------

Self explaining web pages to test without a fully dressed web client.
Examine cookies with developer tools of a browser.
See the test classes in the servlets package for expected behaviour.