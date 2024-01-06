# Demographics summary
# ------------------------------------------------------------------------------
# Create folder to save demographics summary tables
demog.sum.func <- function(){
  demog.path <<- paste(glob.wd, "Demog-Sum", sep = "/")
  if(!dir.exists(paths = demog.path)){
    message(paste("Creating folder for demographics summaries...", sep = ""))
    dir.create(path = demog.path)
  } # if
  setwd(dir = demog.path)

  # Remove duplicate patients, if any
  demog <- obsdf %>% dplyr::filter(!duplicated(UID2))

  #Check the reference number
  if(con.model.ref == "Yes" &exists("con.ref")){
    if(con.ref$already.adjusted.in.data == T){
      for(vars in names(con.ref$ref))
        demog[,vars] <- demog[,vars] + con.ref$ref[vars]
    }
  }else if(con.model.ref == "Yes" ){
    con.ref <<- list(ref = round(unlist(con.ref.df["med", names(full.con)]),2),
                    already.adjusted.in.data = F)
    names(con.ref$ref) <<- names(full.con)
  }else{

  }

  # Round continuous variables to 1 decimal place
  demog <- demog %>%
    mutate_if(is.numeric, round, digits = 1)
  demog <<- demog
  # Patient summary by Protocol
  if(exists("full.cat")){
    if("PROT" %in% names(x = full.cat)){
      prot.var <<- tail(x = full.cat$PROT, 1)
    } else{ # if
      prot.var <<- "PROT"
    } # else
  } else{ # if
    prot.var <<- "PROT"
  } # else
  # Make additional grouping variable PROT if not declared in user_input
  if(!exists("demog_grp_var")) {demog_grp_var <<- prot.var}
  if(!exists("demog_grp_var_label")) {demog_grp_var_label <<- demog_grp_var}

  demog_tot <<- tot_sum.func(df = demog, groups = demog_grp_var_label)

  if(summary_covs == "Yes"){
    # Summary for each study
    if(!is.null(orig.con)){
      conSum <<- get_cont_sum_prot.func(df = demog, groups = demog_grp_var_label)
      # Export continuous demographics table
      demog_cont <<- bind_rows(demog_tot, conSum)
      indx <<- which(x = demog_cont[, 2] == "")
      df_to_latex.func(demog_cont, grouping_rows = indx, file = "Demographics-continuous.tex", LaTex = LaTex.table)
    }

    if(!is.null(full.cat)){
      catSum <<- get_cat_sum_prot.func(df = demog, groups = demog_grp_var_label)
      # Export categorical demographics table
      demog_cat <<- bind_rows(demog_tot, catSum)
      indx <<- which(x = demog_cat[, 2] == "")
      df_to_latex.func(demog_cat, grouping_rows = indx, file = "Demographics-categorical.tex", LaTex = LaTex.table)
    }
  } # if

  # Summarize by Endpoint and Additional Grouping Variable (Protocol is Default)

  df.sum.protocol1 <<- obsdf %>%
    distinct_at(c("UID2", endpcolName, demog_grp_var_label), .keep_all = T) %>%
    group_by(across(.cols = all_of(x = c(endpcolName, demog_grp_var_label)))) %>%
    summarise(Total = n_distinct(UID2, na.rm = F),
              Events = n_distinct(UID2[across(all_of(dvf)) == "Yes"], na.rm = T),
              NoEvents = n_distinct(UID2[across(all_of(dvf)) == "No"], na.rm = T),
              Missing = n_distinct(UID2[is.na(across(all_of(dvf)))])) %>%
    mutate(Events = paste0(Events, " (", round(x = Events/Total*100, digits = 1),")"),
           NoEvents = paste0(NoEvents, " (", round(x = NoEvents/Total*100, digits = 1), ")"),
           Missing = paste0(Missing, " (", round(x = Missing/Total*100, digits = 1), ")")) %>%
    dplyr::select_at(c(endpcolName, demog_grp_var_label, "Events", "NoEvents", "Missing")) %>%
    dplyr::rename(grp = all_of(x = demog_grp_var_label)) %>%
    ungroup()

  # Summarize Total (by Endpoint)
  df.sum.total1 <<- obsdf %>%
    distinct_at(c("UID2", endpcolName, demog_grp_var_label), .keep_all = T) %>%
    group_by(across(.cols = all_of(x = c(endpcolName)))) %>%
    summarise(Total = n_distinct(UID2, na.rm = F),
              Events = n_distinct(UID2[across(all_of(dvf)) == "Yes"], na.rm = T),
              NoEvents = n_distinct(UID2[across(all_of(dvf)) == "No"], na.rm = T),
              Missing = n_distinct(UID2[is.na(across(all_of(dvf)))])) %>%
    mutate(Events = paste0(Events, " (", round(x = Events/Total*100, digits = 1), ")"),
           NoEvents = paste0(NoEvents, " (", round(x = NoEvents/Total*100, digits = 1), ")"),
           Missing = paste0(Missing, " (", round(x = Missing/Total*100, digits = 1), ")"),
           grp = "Total") %>%
    dplyr::select_at(c(endpcolName, "grp", "Events", "NoEvents", "Missing")) %>%
    ungroup()

  # Summarize Total (by Endpoint) for Final Endpoint Consideration
  df.sum.total2 <<- obsdf %>%
    group_by(across(.cols = all_of(x = c(endpcolName)))) %>%
    summarise(Total = n_distinct(UID2, na.rm = F),
              Events = n_distinct(UID2[across(all_of(dvf)) == "Yes"], na.rm = T),
              Events_per = ".",
              NoEvents = n_distinct(UID2[across(all_of(dvf)) == "No"], na.rm = T),
              NoEvents_per = ".",
              Missing = n_distinct(UID2[is.na(across(all_of(dvf)))]),
              Missing_per = ".") %>%
    mutate(Events_per = round(x = Events/Total*100, digits = 1),
           NoEvents_per = round(x = NoEvents/Total*100, digits = 1),
           Missing_per = round(x = Missing/Total*100, digits = 1),
           grp = "Total") %>%
    dplyr::select_at(c(endpcolName, "grp", "Events", "Events_per", "NoEvents", "NoEvents_per", "Missing", "Missing_per")) %>%
    ungroup()

  # Combine Endpoint, Additional Grouping Variable (Protocol is Default), total summaries
  df.sum2 <- data.table::rbindlist(list(df.sum.protocol1, df.sum.total1), use.names = T) %>%
    arrange_at(c(endpcolName, "grp")) %>%
    pivot_longer(cols = c("Events", "NoEvents", "Missing")) %>%
    pivot_wider(names_from = grp)

  ##Set Total column to be the last column
  df.sum2 <- cbind(df.sum2%>%dplyr::select(-Total), df.sum2%>%dplyr::select(Total))
  df.sum2[is.na(df.sum2)] <- "-"

  # Format Endpoint grouping
  df.sum2 <- split(df.sum2, df.sum2[,endpcolName]) %>%
    map2(.x =., .y = unname(obj = sapply(X = endpName, "[[", 2)), .f = function(x, y){
      #map2(.x =., .y = unname(obj = sapply(X = endpName, "[[", 2)[order(x = names(endpName))]), .f = function(x, y){
      tmp <- x %>%
        dplyr::select_at(vars(-endpcolName))
      tmp$name <- c("Events (\\%)", "No Events (\\%)", "Missing (\\%)")
      varcol <- ""
      tmp[colnames(x = tmp)] <- lapply(X = tmp[colnames(x = tmp)], FUN = as.character)
      tmp <- rbind(varcol, tmp)
      tmp$name[1] <- y
      colnames(tmp)[1] <- "Statistics"
      return(tmp)
    } # function(x, y)
    ) # map2

  # Add total subjects
  N <- obsdf %>% group_by(across(.cols = all_of(x = demog_grp_var_label))) %>%
    summarise(N = n_distinct(UID2)) %>% pull(N)
  N <- c("N", N, sum(N))
  df.sum2 <- data.table::rbindlist(df.sum2, fill = T)
  names(N) <- names(df.sum2)
  df.sum2 <- bind_rows(N,  mutate_all(.tbl = df.sum2, .funs = as.character))
  df.sum2$Statistics <- convert_express_latex.func(text_to_convert = df.sum2$Statistics)
  print(df.sum2)

  indx <- which(x = df.sum2[[2]] == "")
  df_to_latex.func(df.sum2, grouping_rows = indx, file = "EventsByProtocol.tex", LaTex = LaTex.table)
  df.sum2 <<- df.sum2

  # Calculate the # of rows for each endpoint. The first row is always the total.
  p <- (nrow(df.sum2) - 1)/length(endpoints)
  for(i in 1:length(endpoints)){
    df.sum.endpoint <- df.sum2[c(1,(2+(i-1)*p):(1+i*p)),]
    df.sum.endpoint$Statistics[df.sum.endpoint$Statistics=="N"] <- "Total Number of Individuals"
    indx.endpoint <- which(x = df.sum.endpoint[[2]] == "")
    if(exists("sub.endpcolName")&exists("sub.endpName")){
      if(names(endpName)[i]%in%names(sub.endpName)){
        flag.df <- obsdf[obsdf[,endpcolName]==endpoints[i],]
        subcat.sum <- flag.df %>% group_by_at(dvf) %>% group_split() %>% map(function(flag.sub){
          a <- flag.sub %>% select_at(c(sub.endpcolName, demog_grp_var_label)) %>% table()
          sub.labels <- sub.endpName[[which(names(sub.endpName)==names(endpName)[i])]]
          sub.labels <- rownames(a) %>% map(function(x){names(sub.labels)[sub.labels == x]}) %>% unlist()
          a <- a %>% cbind(Statistics = sub.labels, Total = rep("",nrow(a)))
          return(list(flag.sub[1,dvf], a))
        })
        df.sum.endpoint <- rbind(df.sum.endpoint[1:(indx.endpoint),],
                                 c("Event = Yes", rep("", ncol(df.sum.endpoint)-1)),
                                 subcat.sum[[which(unlist(sapply(subcat.sum, FUN = function(x)x[[1]][1]))=="Yes")]][[2]],
                                 c("Event = No", rep("", ncol(df.sum.endpoint)-1)),
                                 subcat.sum[[which(unlist(sapply(subcat.sum, FUN = function(x)x[[1]][1]))=="No")]][[2]],
                                 df.sum.endpoint[(indx.endpoint+3):nrow(df.sum.endpoint),])

      }
    }
    df_to_latex.func(df.sum.endpoint, grouping_rows = indx.endpoint, file = paste0("Model_EventsByProtocol_endpoints_",endpoints[i],".tex"), LaTex = LaTex.table)
  }

  # Summarize Endpoint grades by Additional Grouping Variable (Protocol is Default)
  if(exists("dvg")){
    df.grade.protocol <- obsdf %>%
      group_by(across(.cols = all_of(x = c("Endpoint", demog_grp_var_label, dvg)))) %>%
      summarise(Total = n_distinct(UID2, na.rm = F)) %>%
      mutate(N = sum(Total),
             freq = Total / sum(Total) * 100,
             freq = round(x = freq, digits = 1)) %>%
      mutate(Total = paste0(Total, " (", freq, ")")) %>%
      dplyr::select(-freq) %>%
      spread(key = dvg, value = Total) %>%
      replace(is.na(.), "0 (0)") %>%
      ungroup() %>%
      mutate_at(demog_grp_var_label, as.character)

    # Format Endpoint grouping
    df.grade.protocol <- split(df.grade.protocol, df.grade.protocol$Endpoint) %>%
      map2(.x =., .y = unname(obj = sapply(X = endpName, "[[", 2)), .f = function(x, y){
        tmp <- x %>%
          dplyr::select(-Endpoint)
        varcol <- ""
        tmp <- rbind(varcol, tmp)
        tmp[1,1] <- y
        grade.cols <- colnames(tmp)[colnames(tmp) != demog_grp_var & colnames(tmp) != "N" & colnames(tmp) != "<NA>"]
        grade.cols <- paste("Grade ", grade.cols, sep = "")
        colnames(tmp) <- c("Protocol", "N", grade.cols, "Missing")
        return(tmp)
      } # function(x, y)
      ) # map2
    df.grade.protocol <- data.table::rbindlist(df.grade.protocol, fill = T)
    # Grouping variable row number
    indx <- which(x = df.grade.protocol[[2]] == "")
    df_to_latex.func(df.grade.protocol, grouping_rows = indx, file = "GradesByProtocol.tex", LaTex = LaTex.table)
    df.grade.protocol <<- df.grade.protocol
    GradesByProtocol <<- T
  }else{
    GradesByProtocol <<- F
  }

  # Summary of Exposure by Additional Grouping Variable (Protocol is Default)
  print(head(demog))
  expo.sum <- pk_sum_pop_all.func(df = demog,
                                  groups = demog_grp_var_label,
                                  parameters = names(x = exposureCov),
                                  parameters_label = unname(obj = exposureCov))
  expo.sum <- bind_rows(demog_tot, expo.sum)
  # Grouping variable row number
  indx <- which(x = expo.sum[[2]] == "")
  print(expo.sum)
  expo.sum <<- expo.sum
  df_to_latex.func(df = expo.sum, grouping_rows = indx, file = "ExposureByProtocol.tex", LaTex = LaTex.table)

  message("Demographic summaries complete")

}
