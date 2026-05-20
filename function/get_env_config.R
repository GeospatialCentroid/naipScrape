#' Get Environment Configuration for NAIP Processing
#'
#' @param MAC Logical. If TRUE, loads macOS paths and limits. If FALSE, loads Ubuntu local 10Gb paths.
#' @return A list of configuration parameters.
get_env_config <- function(MAC = FALSE) {
  
  config_path <- "config.yml"
  
  if (!file.exists(config_path)) {
    stop("config.yml is missing. Please create it and define 'truenas_ip'.")
  }
  
  # Read the YAML file
  app_config <- yaml::read_yaml(config_path)
  server_ip <- app_config$server$truenas_ip
  
  if (is.null(server_ip) || server_ip == "") {
    stop("truenas_ip is not set correctly in config.yml.")
  }
  
  if (MAC) {
    # ---------------------------------------------------------
    # macOS Remote (Tailscale) Configuration
    # ---------------------------------------------------------
    list(
      os_env              = "macOS",
      mount_point         = "/Volumes/fileShare",
      network_storage_dir = "/Volumes/fileShare/NAIP",
      mount_cmd           = sprintf("mkdir -p /Volumes/fileShare && mount -t smbfs //guest:@%s/fileShare /Volumes/fileShare", server_ip),
      # bwlimit             = "20M", 
      workers             = max(1, future::availableCores() - 2)
    )
  } else {
    # ---------------------------------------------------------
    # Ubuntu VM Local (10GbE) Configuration
    # ---------------------------------------------------------
    list(
      os_env              = "Ubuntu",
      mount_point         = "/home/dune/trueNAS/work/naipScrape/mnt/fileShare",
      network_storage_dir = "/home/dune/trueNAS/work/naipScrape/mnt/fileShare/NAIP",
      mount_cmd           = sprintf("sudo mount -t cifs -o guest,uid=$(id -u),gid=$(id -g) //%s/fileShare /home/dune/trueNAS/work/naipScrape/mnt/fileShare", server_ip),
      bwlimit             = "700M",
      workers             = 28
    )
  }
}
