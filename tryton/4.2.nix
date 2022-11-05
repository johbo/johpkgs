{ pkgs, stdenv, fetchurl, fetchgit, python2Packages
, withPostgresql ? true }:

with stdenv.lib;

let

  modulesGenerated = import ./modules.nix {
    inherit pkgs fetchurl fetchgit;
  };

  localOverrides = self: super: {

    # Alias names to line up with PyPI
    Genshi = super.genshi;
    python-dateutil = super.dateutil;
    PyPDF2 = super.pypdf2;

    trytond = self.buildPythonPackage rec {
      name = "trytond-${version}";
      version = "4.2.1";
      src = fetchurl {
        url = "mirror://pypi/t/trytond/${name}.tar.gz";
        sha256 = "1ijjpbsf3s0s7ksbi7xgzss4jgr14q5hqsyf6d68l8hwardrwpj7";
      };

      # Tells the tests which database to use
      DB_NAME = ":memory:";

      buildInputs = with self; [
        mock
      ];
      propagatedBuildInputs = with self; ([
        dateutil
        lxml
        polib
        python-sql
        relatorio
        werkzeug
        wrapt

        # extra dependencies
        bcrypt
        pydot
        python-Levenshtein
        simplejson
      ] ++ optional withPostgresql psycopg2);

      meta = {
        description = "The server of the Tryton application platform";
        longDescription = ''
          The server for Tryton, a three-tier high-level general purpose
          application platform under the license GPL-3 written in Python and using
          PostgreSQL as database engine.

          It is the core base of a complete business solution providing
          modularity, scalability and security.
        '';
        homepage = http://www.tryton.org/;
        license = licenses.gpl3Plus;
        maintainers = [ maintainers.johbo ];
      };
    };

    trytondModules = with self; [
      trytond
      trytond-account
      trytond-account-asset
      trytond-account-be
      trytond-account-credit-limit
      trytond-account-deposit
      trytond-account-de-skr03
      trytond-account-dunning
      trytond-account-dunning-fee
      trytond-account-dunning-letter
      trytond-account-fr
      trytond-account-invoice
      trytond-account-invoice-history
      trytond-account-invoice-line-standalone
      trytond-account-invoice-stock
      trytond-account-payment
      trytond-account-payment-clearing
      trytond-account-payment-sepa
      trytond-account-payment-sepa-cfonb
      trytond-account-product
      trytond-account-statement
      trytond-account-stock-anglo-saxon
      trytond-account-stock-continental
      trytond-account-stock-landed-cost
      trytond-account-stock-landed-cost-weight
      trytond-account-tax-rule-country
      trytond-analytic-account
      trytond-analytic-invoice
      trytond-analytic-purchase
      trytond-analytic-sale
      trytond-authentication-sms
      trytond-bank
      trytond-calendar
      trytond-calendar-classification
      trytond-calendar-scheduling
      trytond-calendar-todo
      trytond-carrier
      trytond-carrier-percentage
      trytond-carrier-weight
      trytond-commission
      trytond-commission-waiting
      trytond-company
      trytond-company-work-time
      trytond-country
      trytond-currency
      trytond-customs
      trytond-dashboard
      trytond-google-maps
      # TODO: trouble installing on darwin
      # trytond-ldap-authentication
      # trytond-ldap-connection
      trytond-party
      trytond-party-relationship
      trytond-party-siret
      trytond-party-vcarddav
      trytond-product
      trytond-product-attribute
      trytond-product-classification
      trytond-product-classification-taxonomic
      trytond-product-cost-fifo
      trytond-product-cost-history
      trytond-production
      trytond-production-routing
      trytond-production-split
      trytond-production-work
      trytond-production-work-timesheet
      trytond-product-measurements
      trytond-product-price-list
      trytond-project
      trytond-project-invoice
      trytond-project-plan
      trytond-project-revenue
      trytond-purchase
      trytond-purchase-invoice-line-standalone
      trytond-purchase-request
      trytond-purchase-shipment-cost
      trytond-sale
      trytond-sale-complaint
      trytond-sale-credit-limit
      trytond-sale-extra
      trytond-sale-invoice-grouping
      trytond-sale-opportunity
      trytond-sale-price-list
      trytond-sale-promotion
      trytond-sale-shipment-cost
      trytond-sale-shipment-grouping
      trytond-sale-stock-quantity
      trytond-sale-supply
      trytond-sale-supply-drop-shipment
      trytond-stock
      trytond-stock-forecast
      trytond-stock-inventory-location
      trytond-stock-location-sequence
      trytond-stock-lot
      trytond-stock-lot-sled
      trytond-stock-package
      trytond-stock-package-shipping
      trytond-stock-package-shipping-dpd
      # TODO: Fix up naming clash requests vs requests2
      # trytond-stock-package-shipping-ups
      trytond-stock-product-location
      trytond-stock-split
      trytond-stock-supply
      trytond-stock-supply-day
      trytond-stock-supply-forecast
      trytond-stock-supply-production
      trytond-timesheet
      trytond-timesheet-cost
      trytond-webdav
      trytond-web-user
    ];

    trytondEnv = self.python.buildEnv.override {
      extraLibs = self.trytondModules;
    };

  };

  modulesUnfix =
    (extends localOverrides
    (extends modulesGenerated
             python2Packages.__unfix__));

  modules = (fix' modulesUnfix);

in modules.trytondEnv // {
  inherit modules modulesUnfix;
}
