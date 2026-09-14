# Package index

## Core calculation

Single-employee PAYE, take-home pay, and statutory reliefs.

- [`calc_paye()`](https://laws2020.github.io/nrsR/reference/calc_paye.md)
  : Calculate Nigerian PAYE tax for one employee
- [`calc_net_salary()`](https://laws2020.github.io/nrsR/reference/calc_net_salary.md)
  : Calculate monthly net (take-home) salary
- [`calc_reliefs()`](https://laws2020.github.io/nrsR/reference/calc_reliefs.md)
  : Calculate statutory tax reliefs for an employee

## Payroll & comparison

Batch payroll, cross-law comparison, and band detail.

- [`calc_paye_batch()`](https://laws2020.github.io/nrsR/reference/calc_paye_batch.md)
  : Calculate PAYE for a batch of employees
- [`compare_tax_laws()`](https://laws2020.github.io/nrsR/reference/compare_tax_laws.md)
  : Compare tax liability across any number of registered law versions
- [`tax_breakdown()`](https://laws2020.github.io/nrsR/reference/tax_breakdown.md)
  : Detailed tax band breakdown

## Law registry

Inspect and resolve the versioned tax-law registry.

- [`list_tax_laws()`](https://laws2020.github.io/nrsR/reference/list_tax_laws.md)
  : List all registered tax laws
- [`get_applicable_law()`](https://laws2020.github.io/nrsR/reference/get_applicable_law.md)
  : Get the tax law applicable on a given date

## Utilities

Formatting helpers.

- [`format_naira()`](https://laws2020.github.io/nrsR/reference/format_naira.md)
  : Format a number as Nigerian Naira

## Print & summary methods

S3 methods for the result objects.

- [`print(`*`<nigeria_tax>`*`)`](https://laws2020.github.io/nrsR/reference/print.nigeria_tax.md)
  : Print a Nigerian PAYE tax computation
- [`print(`*`<nigeria_net>`*`)`](https://laws2020.github.io/nrsR/reference/print.nigeria_net.md)
  : Print a monthly payslip summary
- [`print(`*`<nigeria_comparison>`*`)`](https://laws2020.github.io/nrsR/reference/print.nigeria_comparison.md)
  : Print a side-by-side tax law comparison
- [`print(`*`<nigeria_reliefs>`*`)`](https://laws2020.github.io/nrsR/reference/print.nigeria_reliefs.md)
  : Print a Nigerian reliefs breakdown
- [`summary(`*`<nigeria_tax>`*`)`](https://laws2020.github.io/nrsR/reference/summary.nigeria_tax.md)
  : Summarise a Nigerian PAYE tax computation
