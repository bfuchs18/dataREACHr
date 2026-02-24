#' util_foodview_summary: Get summary data from individual participants for the Food View task
#'
#' This function calculates summary performance data for an individual participant
#'
#'
#' @param ind_dat Processed individual dataset from rawdata for the Food View task
#' @param format data format to return. 'wide' will return summary statistics by commercial condition (food vs toy), 'long' will return data by run and block. Defualt = 'wide'
#'
#' @return a data.frame with summary performance and task metrics for a participant
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' foodview_summary_beh <- util_foodview_summary(ind_dat)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_foodview_summary <- function(ind_dat, format = 'wide') {

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(ind_dat)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(ind_dat)) {
      stop('ind_dat must be entered as a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('ind_dat must be entered as a data.frame')
  }

  #### Condiiton summary stats function ####

  cond_sum_stats <- function(img_dat, cond, format){

    # subset images in condition
    cond_img_dat <- img_dat[img_dat['commercial_cond'] == cond, ]

    resp_dat <- cond_img_dat[cond_img_dat['response'] == 1 | cond_img_dat['response'] == 2, ]

    n_image = nrow(cond_img_dat)
    n_resp = nrow(resp_dat)
    n_want = nrow(cond_img_dat[cond_img_dat['response'] == 1, ])

    p_resp = nrow(resp_dat) / n_image
    p_want = n_want / n_resp
    avg_rt = mean(as.numeric(resp_dat[['response_time']]))
    med_rt = median(as.numeric(resp_dat[['response_time']]))

    if (format == 'wide'){
      # high ed
      hed_rows <- cond_img_dat[cond_img_dat$food_ed == 'high', ]
      hed_resp_dat <- hed_rows[hed_rows['response'] == 1 | hed_rows['response'] == 2, ]

      hed_n_image = nrow(hed_rows)
      hed_n_resp = nrow(hed_resp_dat)
      hed_n_want = nrow(hed_rows[hed_rows['response'] == 1, ])

      hed_p_resp = nrow(hed_resp_dat) / hed_n_image
      hed_p_want =  hed_n_want / hed_n_resp
      hed_avg_rt = mean(as.numeric(hed_resp_dat[['response']]))
      hed_med_rt = median(as.numeric(hed_resp_dat[['response']]))

      # low ed
      led_rows <- cond_img_dat[cond_img_dat$food_ed == 'low', ]
      led_resp_dat <- led_rows[led_rows['response'] == 1 | led_rows['response'] == 2, ]

      led_n_image = nrow(led_rows)
      led_n_resp = nrow(led_resp_dat)
      led_n_want = nrow(led_rows[led_rows['response'] == 1, ])

      led_p_resp = nrow(led_resp_dat) / led_n_image
      led_p_want = led_n_want / led_n_resp
      led_avg_rt = mean(as.numeric(led_resp_dat[['response']]))
      led_med_rt = median(as.numeric(led_resp_dat[['response']]))

      # sweet
      sweet_rows <- cond_img_dat[cond_img_dat$food_taste == 'sweet',]

      sweet_resp_dat <- sweet_rows[sweet_rows['response'] == 1 | sweet_rows['response'] == 2, ]

      sweet_n_image = nrow(sweet_rows)
      sweet_n_resp = nrow(sweet_resp_dat)
      sweet_n_want = nrow(sweet_rows[sweet_rows['response'] == 1, ])

      sweet_p_resp = nrow(sweet_resp_dat) / sweet_n_image
      sweet_p_want = sweet_n_want / sweet_n_resp
      sweet_avg_rt = mean(as.numeric(sweet_resp_dat[['response']]))
      sweet_med_rt = median(as.numeric(sweet_resp_dat[['response']]))

      # savory
      savory_rows <- cond_img_dat[cond_img_dat$food_taste == 'savory',]

      savory_resp_dat <- savory_rows[savory_rows['response'] == 1 | savory_rows['response'] == 2, ]

      savory_n_image = nrow(savory_rows)
      savory_n_resp = nrow(savory_resp_dat)
      savory_n_want = nrow(savory_rows[savory_rows['response'] == 1, ])

      savory_p_resp = nrow(savory_resp_dat) / savory_n_image
      savory_p_want = savory_n_want / savory_n_resp
      savory_avg_rt = mean(as.numeric(savory_resp_dat[['response']]))
      savory_med_rt = median(as.numeric(savory_resp_dat[['response']]))

      # summarize data across task for condition
      cond_summary_row <- data.frame(
        participant_id = img_dat[1, 'sub'],
        session_id = 'ses-1',
        commerical_cond = cond,

        n_image = n_image,
        n_resp = n_resp,
        n_want = n_want,

        p_resp = p_resp,
        p_want = p_want,
        avg_rt = avg_rt,
        med_rt = med_rt,

        hed_n_image = hed_n_image,
        hed_n_resp = hed_n_resp,
        hed_n_want = hed_n_want,

        hed_p_resp = hed_p_resp,
        hed_p_want = hed_p_want,
        hed_avg_rt = hed_avg_rt,
        hed_med_rt = hed_med_rt,

        led_n_image = led_n_image,
        led_n_resp = led_n_resp,
        led_n_want = led_n_want,

        led_p_resp = led_p_resp,
        led_p_want = led_p_want,
        led_avg_rt = led_avg_rt,
        led_med_rt = led_med_rt,

        sweet_n_image = sweet_n_image,
        sweet_n_resp = sweet_n_resp,
        sweet_n_want = sweet_n_want,

        sweet_p_resp = sweet_p_resp,
        sweet_p_want = sweet_p_want,
        sweet_avg_rt = sweet_avg_rt,
        sweet_med_rt = sweet_med_rt,

        savory_n_image = savory_n_image,
        savory_n_resp = savory_n_resp,
        savory_n_want = savory_n_want,

        savory_p_resp = savory_p_resp,
        savory_p_want = savory_p_want,
        savory_avg_rt = savory_avg_rt,
        savory_med_rt = savory_med_rt
      )
    } else {
      cond_summary_row <- data.frame(
        participant_id = img_dat[1, 'sub'],
        session_id = 'ses-1',
        run_num = img_dat[1, 'run_num'],
        commerical_cond = cond,
        food_ed = img_dat[1, 'food_ed'],
        food_taste = img_dat[1, 'food_taste'],

        n_image = n_image,
        n_resp = n_resp,
        n_want = n_want,

        p_resp = p_resp,
        p_want = p_want,
        avg_rt = avg_rt,
        med_rt = med_rt)
    }

    return(cond_summary_row)
  }


  # clean up ####

  # set response as 0 and RT as NA if rt = 0
  ind_dat[!is.na(ind_dat['response_time']) & ind_dat['response_time'] == 0, 'response'] <- 0
  ind_dat[!is.na(ind_dat['response_time']) & ind_dat['response_time'] == 0, 'response_time'] <- NA


  # on trials with responses of -99, set RT as NA -- -99 indicates responses other than 1 or 2 in first 2 subs (3, 6) due to trigger
  ind_dat[!is.na(ind_dat['response']) & ind_dat['response'] == -99, 'response_time'] <- NA


  # assess response_time outliers
  average_rt = mean(ind_dat[ind_dat['response'] == 1 | ind_dat['response'] == 2, 'response_time'], na.rm = TRUE)

  # Summarize data by advertisement condition in long format ----

  # subset image rows from ind_dat
  jpeg_rows <- ind_dat[grep('jpeg', ind_dat[['stim_file_name']]), ]

  cond_sum_dat <- do.call(rbind.data.frame, t(sapply(unique(jpeg_rows[['commercial_cond']]), function(x) cond_sum_stats(jpeg_rows, x, format), simplify = FALSE)))


  return(cond_sum_dat)
}

