JSON files for manual tests
---------------------------

The files with `datasetmetadata-from-ui-` in their name are derived from the mockserver of [easy-deposit-ui].

All `json` files could be used for manual tests. The expected results are documented with unit tests such as `DatasetXmlSpec`.

`servlet\IntegrationSpec` specifies only some scenarios that could be performed by the UI or manual tests.
Other tests in the `servlets` package emulate just a single step.


Purpose of HTML files
---------------------

Self explaining web pages to test without a fully dressed web client.
Examine cookies with developer tools of a browser.
See the test classes in the servlets package for expected behaviour.

Manual tests
------------

The steps of a deposit scenario can be tested manually with tools like `Postman` or `curl`.

* Launch the `deasy` VM
  * go to the root of [easy-dtap]
  * Make sure the project is up-to-date and `easy-dtap/deasy_base_box` has the latest [base-box]
  * execute the command `vagrant up --no-provision` 
* build the project and deploy it on a running `deasy`

      cd ../easy-deposit-api
      mvn clean install
      cd ../easy-dtap
      ./deploy-role easy-deposit-api

* Create a new deposit:

      curl -i -u user001:user001 -X POST http://deasy:20190/deposit

* Use the returned UUID to replace <UUID> in the command below (that uploads dataset metadata),
  use the files in this directory for XXX
  and play with the content to check whether responses make sense for the UI and user of the UI.


      curl -i  -H 'Content-Type: application/json' --data-binary "@XXX.json" -X PUT -u user001:user001 'http://deasy.dans.knaw.nl:20190/deposit/<UUID>/datasetmetadata'

* For details of other commands see `docs/api.html`, with examples in the [xls of EASY-1644] and [easy-test-resources]

Reserve a DOI or submit requires a pid generator which is not deployed on the local VM.
For other commands you can use the local `test` VM
which you can launch with `vagrant up` in the root of the project. 

[easy-test-resources]: https://github.com/DANS-KNAW/easy-test-resources/blob/master/test-run/EASY-1525-deposit-api.md
[xls of EASY-1644]: https://drivenbydata.atlassian.net/secure/attachment/25376/2018-08-03%20EASY-1644%20Deposit_API_1.0.0.xlsx
[base-box]: http://develop.dans.knaw.nl/boxes/
[easy-deposit-ui]: https://github.com/DANS-KNAW/easy-deposit-ui
[easy-dtap]: https://github.com/DANS-KNAW/easy-dtap