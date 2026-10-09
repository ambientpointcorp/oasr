# the returned structure of every entry point is unchanged

    Code
      cat(shape, sep = "\n")
    Output
      ## calculate_credit_vectors
      $prepay_rates : array [4 x 361 x 12]
      $default_rates : array [4 x 361 x 12]
      $recovery_rates : array [4 x 361 x 12]
      $recovery_rate : numeric [1]
      $recovery_lag : numeric [1]
      ## project_cashflows[[1]]
       : array [361 x 9 x 12]
      ## project_cashflows_with_io[[1]]
       : array [361 x 10 x 12]
      ## aggregate_cashflows[[1]]
       : array [361 x 5 x 3]
      ## calculate_present_value
      $present_values : matrix/array [4 x 12]
      $mean_pvs : numeric [12]
      $pv_stats : data.frame [12 x 6] {item, mean_pv, sd_pv, min_pv, max_pv, cv_pv}
      ## calculate_average_life
      $avg_lives$by_asset : numeric [12]
      $avg_lives$overall_wal : numeric [1]
      $avg_lives_by_path : matrix/array [4 x 12]
      ## calculate_duration_convexity
      $duration : numeric [12]
      $convexity : numeric [12]
      $price_up : numeric [12]
      $price_base : numeric [12]
      $price_down : numeric [12]
      ## calculate_oas
      $price : numeric [1]
      $oas : numeric [1]
      $pv_result$present_values : matrix/array [4 x 12]
      $pv_result$mean_pvs : numeric [12]
      $pv_result$pv_stats : data.frame [12 x 6] {item, mean_pv, sd_pv, min_pv, max_pv, cv_pv}
      $avg_life$avg_lives$by_asset : numeric [12]
      $avg_life$avg_lives$overall_wal : numeric [1]
      $avg_life$avg_lives_by_path : matrix/array [4 x 12]
      $duration$duration : numeric [12]
      $duration$convexity : numeric [12]
      $duration$price_up : numeric [12]
      $duration$price_base : numeric [12]
      $duration$price_down : numeric [12]
      $structured_cashflows : NULL [0]
      $market_price : numeric [1]
      $price_error : numeric [1]
      $converged : logical [1]
      $iterations : numeric [1]
      $search_range : numeric [2]
      $options$reference_key : character [1]
      $options$discounting : character [1]
      $options$accrual_method : character [1]
      $options$start_date : NULL [0]
      $options$terminal_balance : character [1]
      $options$recovery_basis : character [1]
      ## summary.oas_result
      $overall : data.frame [7 x 2] {Metric, Value}
      $convergence$converged : logical [1]
      $convergence$iterations : numeric [1]
      $execution_time : difftime [1]
      $asset_info : data.frame [12 x 7] {Asset, Balance, WAC, WAL.by_asset, WAL.overall_wal, Duration, Convexity}

# headline figures are unchanged

    Code
      print(figures, row.names = FALSE)
    Output
                    metric        value
       scheduled principal 2016281.0000
                prepayment 1238195.0000
                   default  110266.0000
                  recovery   71281.0000
                  interest 1243362.0000
                 servicing   57109.0000
                     total 4512009.0000
          mean PV at 100bp 3754084.0000
               WAL (years)       6.0530
                  duration      -5.1366
                 convexity      38.1540

# the legacy conventions still reproduce the pre-Phase-4 figures

    Code
      print(figures, row.names = FALSE)
    Output
                       metric       value
          scheduled principal 2016281.000
                     recovery   60571.000
                     interest 1243362.000
                        total 4501299.000
       mean PV at 100bp, spot 3563219.000
                  WAL (years)       6.053

