# PAYE from First Principles: A Deep Dive with nrsR

## 1. Introduction

Every month, millions of Nigerian workers see a line on their payslip  
labelled **PAYE** — money withheld before they are paid. Employers are  
legally responsible for computing that figure correctly, and getting
it  
wrong means either short-changing the employee or under-remitting to
the  
**Nigeria Revenue Service (NRS)**.

`nrsR` exists to make that computation **exact, reproducible, and  
future-proof**. This vignette takes you from “what is PAYE?” all the way
to  
running payroll for an entire workforce — and shows why `nrsR`’s
design  
means a 2028 tax amendment will cost you *one file edit*, not a rewrite.

## 2. Definition

**PAYE (Pay-As-You-Earn)** is a system where income tax is deducted at  
source, spread across the year rather than paid in one lump sum. In
Nigeria  
the amount owed is **progressive**: income is sliced into bands, and
each  
band is taxed at its own rate.

Two laws matter today:

``` r

list_tax_laws()  
#>             key                                   description effective_from
#> PITA       PITA              Personal Income Tax Act (legacy)     1993-01-01
#> NTA2025 NTA2025 Nigeria Tax Act 2025 (effective January 2026)     2026-01-01
#>         effective_to n_bands
#> PITA      2025-12-31       6
#> NTA2025      current       6
```

| Term | Meaning in `nrsR` |
|----|----|
| **Gross** | Total monthly pay before any deduction |
| **Relief** | Amounts subtracted before tax (pension, NHF, NHIS, rent relief, CRA) |
| **Taxable income** | Gross annual minus total reliefs |
| **Band** | An income slice with a fixed rate |
| **Law registry** | The single source of truth for all bands, rates, and reliefs |

## 3. Storytelling

Meet **Ngozi**, a Lagos product manager earning **₦1,624,734/month**
who  
pays **₦1,800,000/year** in rent. In December 2025 she is taxed under
the  
old **PITA** regime. On 1 January 2026 the **Nigeria Tax Act 2025  
(NTA2025)** takes effect — the first ₦800,000 of annual income becomes  
tax-free, the old Consolidated Relief Allowance (CRA) disappears, and a
new  
**rent relief** appears.

Ngozi wants to know: *“Will I take home more or less next year?”* With  
`nrsR`, she doesn’t guess — she computes both worlds and compares them.

``` r

compare_tax_laws(1624734, annual_rent = 1800000)  
#> 
#> ======================================================================
#>  TAX LAW COMPARISON
#> ======================================================================
#>  Metric                      PITA                NTA2025            
#> ----------------------------------------------------------------------
#>  Annual Relief               ₦6,146,526.44     ₦2,407,164.84    
#>  Taxable Income (Ann.)       ₦13,350,281.56    ₦17,089,643.16   
#>  Annual Tax                  ₦2,996,067.57     ₦3,018,825.06    
#>  Monthly Tax                 ₦249,672.30       ₦251,568.76      
#>  Effective Rate (%)          15.37%              15.48%             
#>  Net Monthly Pay             ₦1,204,464.63     ₦1,202,568.17    
#> ======================================================================
```

The story of `nrsR` is really the story of **change**: tax laws evolve,
and  
the tool is built so that evolution is cheap.

## 4. Illustration / Analogy

Think of progressive tax like **filling a set of stacked buckets**.

Income pours in from the top. The **first bucket** (first ₦800,000
under  
NTA2025) has a tap set to **0%** — nothing drips out as tax. Once it  
overflows, income spills into the **next bucket**, whose tap is open
wider  
(15%), and so on. The higher the bucket, the wider the tap.

You are never taxed at your top rate on *all* your money — only on the  
portion that reaches that bucket.
[`tax_breakdown()`](https://laws2020.github.io/nrsR/reference/tax_breakdown.md)
shows exactly how much  
landed in each bucket and how much dripped out:

``` r

tax_breakdown(1624734, annual_rent = 1800000)  
#>              band rate_pct income_in_band     tax cumulative_tax
#> 1 First ₦800K @0%        0         800000       0              0
#> 2 Next ₦2.2M @15%       15        2200000  330000         330000
#> 3   Next ₦9M @18%       18        9000000 1620000        1950000
#> 4  Next ₦13M @21%       21        5089643 1068825        3018825
#> 5  Next ₦25M @23%       23              0       0        3018825
#> 6 Above ₦50M @25%       25              0       0        3018825
```

## 5. Examples

### One employee, full computation

``` r

t1 <- calc_paye(1624734, annual_rent = 1800000)  
print(t1)  
#> 
#> ==========================================================
#>  NIGERIAN PAYE TAX COMPUTATION
#>  Law: Nigeria Tax Act 2025 (effective January 2026) 
#> ==========================================================
#>  Monthly Gross:                      ₦1,624,734.00
#>  Annual Gross:                       ₦19,496,808.00
#> ----------------------------------------------------------
#>  DEDUCTIONS & RELIEFS
#>    Pension (8%/month):               ₦129,978.72
#>    NHF (2.5%/month):                 ₦40,618.35
#>    Rent Relief (annual):             ₦360,000.00
#>    Total Annual Relief:              ₦2,407,164.84
#> ----------------------------------------------------------
#>  Annual Taxable Income:              ₦17,089,643.16
#> ----------------------------------------------------------
#>  TAX BANDS
#>    First ₦800K @0%            ₦800,000.00  =>  ₦0.00
#>    Next ₦2.2M @15%            ₦2,200,000.00  =>  ₦330,000.00
#>    Next ₦9M @18%              ₦9,000,000.00  =>  ₦1,620,000.00
#>    Next ₦13M @21%             ₦5,089,643.16  =>  ₦1,068,825.06
#> ----------------------------------------------------------
#>  ANNUAL TAX:                         ₦3,018,825.06
#>  MONTHLY TAX:                        ₦251,568.76
#>  Effective Rate (on gross):          15.48%
#> ==========================================================
```

### Take-home pay (the payslip view)

``` r

calc_net_salary(1624734, annual_rent = 1800000)  
#> 
#> ====================================================
#>  MONTHLY PAYSLIP SUMMARY
#> ====================================================
#>  Gross Income:                  ₦1,624,734.00
#> ----------------------------------------------------
#>  DEDUCTIONS
#>    Pension:                     ₦129,978.72
#>    NHF:                         ₦40,618.35
#>    PAYE Tax:                    ₦251,568.76
#> ----------------------------------------------------
#>  Total Deductions:              ₦422,165.83
#> ====================================================
#>  NET TAKE-HOME PAY:             ₦1,202,568.17
#> ====================================================
```

### Just the reliefs

``` r

calc_reliefs(1624734, annual_rent = 1800000)  
#> $gross_monthly
#> [1] 1624734
#> 
#> $gross_annual
#> [1] 19496808
#> 
#> $pension_monthly
#> [1] 129978.7
#> 
#> $pension_annual
#> [1] 1559745
#> 
#> $nhf_monthly
#> [1] 40618.35
#> 
#> $nhf_annual
#> [1] 487420.2
#> 
#> $nhis_monthly
#> [1] 0
#> 
#> $nhis_annual
#> [1] 0
#> 
#> $cra_annual
#> [1] 0
#> 
#> $annual_rent
#> [1] 1800000
#> 
#> $rent_relief_annual
#> [1] 360000
#> 
#> $total_relief_monthly
#> [1] 200597.1
#> 
#> $total_relief_annual
#> [1] 2407165
#> 
#> $taxable_income_annual
#> [1] 17089643
#> 
#> $law
#> [1] "NTA2025"
#> 
#> $law_description
#> [1] "Nigeria Tax Act 2025 (effective January 2026)"
#> 
#> attr(,"class")
#> [1] "nigeria_reliefs" "list"
```

### Let the date pick the law

``` r

get_applicable_law("2025-06-01")   # PITA  
#> [1] "PITA"
get_applicable_law("2026-03-12")   # NTA2025  
#> [1] "NTA2025"
calc_paye(1624734, law = "auto")$law  
#> [1] "NTA2025"
```

### A whole workforce at once

``` r

employees <- data.frame(  
  name          = c("Emeka Obi", "Ngozi Adeyemi", "Tunde Balogun"),  
  gross_monthly = c(693228.96,   1624734,          3500000),  
  annual_rent   = c(0,           1800000,           2400000)  
)  
payroll <- calc_paye_batch(employees)  
payroll[, c("name", "gross_monthly", "monthly_tax",  
            "effective_rate_pct", "net_monthly")]  
#>            name gross_monthly monthly_tax effective_rate_pct net_monthly
#> 1     Emeka Obi        693229    94179.19              13.59    526260.7
#> 2 Ngozi Adeyemi       1624734   251568.76              15.48   1202568.2
#> 3 Tunde Balogun       3500000   622108.33              17.77   2510391.7
```

### Format like a payslip

``` r

format_naira(t1$monthly_tax)  
#> [1] "₦251,568.76"
```

## 6. Insights

- **Effective rate ≠ top band rate.** Ngozi may touch the 18% band, but
  her  
  *effective* rate (tax ÷ gross) is much lower because the lower buckets
  are  
  cheap or free. Read `effective_rate` from
  [`calc_paye()`](https://laws2020.github.io/nrsR/reference/calc_paye.md).  
- **Reliefs are leverage.** Pension (8%) and rent relief directly
  shrink  
  taxable income. Two people with identical gross can owe different
  tax.  
- **NTA2025 helps lower earners most.** The ₦800K tax-free floor
  removes  
  low-income workers from tax entirely — verify by running a small
  salary  
  through both laws.  
- **Auto-dispatch removes human error.** Passing `law = "auto"` means
  your  
  payroll always uses the legally-correct law for the run date.

## 7. Diagram — how a computation flows

            gross_monthly, annual_rent, toggles, law  
                              |  
                              v  
                      +----------------+  
                      |  calc_paye()   |  
                      +----------------+  
                              |  
                 1. resolve law (registry)  
                              |  
                              v  
                      +----------------+  
                      | calc_reliefs() |  pension, NHF, NHIS,  
                      +----------------+  CRA / rent relief  
                              |  
                 2. taxable = gross_annual - reliefs  
                              |  
                              v  
                      +----------------+  
                      | .apply_bands() |  pour income through  
                      +----------------+  the stacked buckets  
                              |  
                 3. sum per-band tax  
                              |  
                              v  
            annual_tax, monthly_tax, effective_rate  

Every arrow reads its numbers from **one place** — `.TAX_LAW_REGISTRY`
in  
`R/law_registry.R`.

## 8. Visual Analogy — the registry as a “law cartridge”

Picture an old games console. The **console** is your calculation
engine  
(`calc_paye`, `calc_reliefs`, `.apply_bands`) — it never changes. Each
tax  
law is a **cartridge** you slot in: `PITA`, `NTA2025`, and one day  
`NTA2028`. Swapping the cartridge changes the whole game (bands,
rates,  
reliefs) without opening up the console.

Adding a future law = **printing a new cartridge** (one list entry).
The  
console instantly plays it:
[`list_tax_laws()`](https://laws2020.github.io/nrsR/reference/list_tax_laws.md),
[`compare_tax_laws()`](https://laws2020.github.io/nrsR/reference/compare_tax_laws.md),  
[`calc_paye_batch()`](https://laws2020.github.io/nrsR/reference/calc_paye_batch.md)
all recognise it automatically.

## 9. Mental Models

Keep three models in your head:

1.  **Stacked buckets** → progressive bands; only overflow reaches the
    next tap.  
2.  **Console + cartridges** → fixed engine, swappable law
    definitions.  
3.  **Funnel** → gross goes in the top, reliefs are subtracted,
    taxable  
    income drops into the buckets, tax comes out the bottom.

If you can picture those three, you understand both PAYE *and* the
`nrsR`  
architecture.

## 10. Quick Recall / Memory Hooks

- **“Buckets, not blankets.”** You’re taxed bucket-by-bucket, not one
  flat  
  blanket rate.  
- **“800 is the floor.”** Under NTA2025, the first ₦800K/year is
  tax-free.  
- **“Reliefs shrink the taxable, not the gross.”** Deductions lower
  what’s  
  taxed, not what you earn.  
- **“One law, one file.”** New amendment? Edit `R/law_registry.R`
  only.  
- **“Auto = accurate.”** `law = "auto"` always picks the right law for
  the date.

### Function cheat-sheet

| I want… | Call |
|----|----|
| Tax for one person | [`calc_paye()`](https://laws2020.github.io/nrsR/reference/calc_paye.md) |
| Take-home pay | [`calc_net_salary()`](https://laws2020.github.io/nrsR/reference/calc_net_salary.md) |
| Just deductions | [`calc_reliefs()`](https://laws2020.github.io/nrsR/reference/calc_reliefs.md) |
| Whole payroll | [`calc_paye_batch()`](https://laws2020.github.io/nrsR/reference/calc_paye_batch.md) |
| Compare laws | [`compare_tax_laws()`](https://laws2020.github.io/nrsR/reference/compare_tax_laws.md) |
| Band-by-band detail | [`tax_breakdown()`](https://laws2020.github.io/nrsR/reference/tax_breakdown.md) |
| See all laws | [`list_tax_laws()`](https://laws2020.github.io/nrsR/reference/list_tax_laws.md) |
| Law for a date | [`get_applicable_law()`](https://laws2020.github.io/nrsR/reference/get_applicable_law.md) |
| Currency string | [`format_naira()`](https://laws2020.github.io/nrsR/reference/format_naira.md) |

## References

- Nigeria Tax Act (NTA) 2025  
- [Nigeria Revenue Service](https://www.nrs.gov.ng)
