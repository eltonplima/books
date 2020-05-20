expect = require('chai').expect;
statement = require('../statement');


describe('Verify bill report', () => {
    let plays;
    beforeEach(() => {
        plays = {
            "hamlet": {
                "name": "Hamlet",
                "type": "tragedy"
            },
            "as-like": {
                "name": "As You Like It",
                "type": "comedy"
            },
            "othello": {
                "name": "Othello",
                "type": "tragedy"
            },
            "i-dont-know": {
                "name": "I don't know",
                "type": "horror"
            },
        }
    });
    describe('When invoice has', () => {
        it('one unknown performance with audience of 100', function () {
            const invoices = {
                "customer": "BigCo",
                "performances": [
                    {
                        "playID": "i-dont-know",
                        "audience": 100
                    }
                ]
            }
            expect(() => statement(invoices, plays)).to.throw(Error, "unknown type: horror");
        });
        it('one tragedy performance with audience greater than 30', function () {
            const invoices = {
                "customer": "BigCo",
                "performances": [
                    {
                        "playID": "hamlet",
                        "audience": 55
                    }
                ]
            }
            const value = statement(invoices, plays);
            expect(value).to.equal("Statement for BigCo\n" +
                "  Hamlet: $650.00(55 seats)\n" +
                "Amount owed is $650.00\n" +
                "You earned 25 credits\n");
        });
        it('one tragedy performance with audience less than 30', function () {
            const invoices = {
                "customer": "BigCo",
                "performances": [
                    {
                        "playID": "hamlet",
                        "audience": 29
                    }
                ]
            }
            const value = statement(invoices, plays);
            expect(value).to.equal("Statement for BigCo\n" +
                "  Hamlet: $400.00(29 seats)\n" +
                "Amount owed is $400.00\n" +
                "You earned 0 credits\n");
        });
        it('one tragedy performance with audience equals 30', function () {
            const invoices = {
                "customer": "BigCo",
                "performances": [
                    {
                        "playID": "hamlet",
                        "audience": 30
                    }
                ]
            }
            const value = statement(invoices, plays);
            expect(value).to.equal("Statement for BigCo\n" +
                "  Hamlet: $400.00(30 seats)\n" +
                "Amount owed is $400.00\n" +
                "You earned 0 credits\n");
        });
        it('one comedy performance with audience greater than 20', function () {
            const invoices = {
                "customer": "BigCo",
                "performances": [
                    {
                        "playID": "as-like",
                        "audience": 55
                    }
                ]
            }
            const value = statement(invoices, plays);
            expect(value).to.equal("Statement for BigCo\n" +
                "  As You Like It: $740.00(55 seats)\n" +
                "Amount owed is $740.00\n" +
                "You earned 36 credits\n");
        });
        it('one comedy performance with audience less than 20', function () {
            const invoices = {
                "customer": "BigCo",
                "performances": [
                    {
                        "playID": "as-like",
                        "audience": 19
                    }
                ]
            }
            const value = statement(invoices, plays);
            expect(value).to.equal("Statement for BigCo\n" +
                "  As You Like It: $357.00(19 seats)\n" +
                "Amount owed is $357.00\n" +
                "You earned 3 credits\n");
        });
    });
})
