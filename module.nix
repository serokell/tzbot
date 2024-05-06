# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0
{ self, serokell-nix, ... }@inputs: { config, pkgs, lib, ... }:
let
  inherit (lib) mkEnableOption mkOption types mkIf mkDefault;
  inherit (serokell-nix.lib.systemd) hardeningProfiles withHardeningProfile;
in
{
  options.services.tzbot = {
    enable = mkEnableOption "tzbot";
    package = mkOption {
      type = types.path;
      default = self.packages.x86_64-linux.tzbot;
    };
    botConfig = mkOption {
      type = types.attrs;
      description = ''
        tzbot config without slack-related tokens
      '';
      default = {
        maxRetries = 3;
        cacheUsersInfo = "3m";
        cacheConversationMembers = "3m";
        feedbackFile = "/var/lib/tzbot/feedback.log";
        cacheReportDialog = "1h";
        inverseHelpUsageChance = 15;
        logLevel = "Info";
      };
    };
    slackAppToken = mkOption {
      type = types.str;
      description = ''
        Slack application token
      '';
    };
    slackBotToken = mkOption {
      type = types.str;
      description = ''
        Bot authentication token
      '';
    };
  };
  config = let cfg = config.services.tzbot; in mkIf cfg.enable {
    systemd.services.tzbot = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      unitConfig.ConditionPathExists = [ cfg.package ];
      script = ''
        export SLACK_TZ_APP_TOKEN="${cfg.slackAppToken}"
        export SLACK_TZ_BOT_TOKEN="${cfg.slackBotToken}"
        ${cfg.package}/bin/tzbot-exe --config ${pkgs.writeText "config.yml" (builtins.toJSON cfg.botConfig)}
      '';

      startLimitBurst = mkDefault 5;
      startLimitIntervalSec = mkDefault 300;
      serviceConfig = withHardeningProfile hardeningProfiles.backend {
        User = "tzbot";
        Group = "tzbot";
        StateDirectory = "tzbot";
        Restart = mkDefault "on-failure";
        RestartSec = mkDefault 10;

        SystemCallFilter = [
          "~@clock"
          "~@debug"
          "~@module"
          "~@mount"
          "~@raw-io"
          "~@reboot"
          "~@swap"
          "~@privileged"
          "~@resources"
          "~@cpu-emulation"
          "~@obsolete"

          # override hardening profile
          "set_mempolicy"
        ];
      };
    };
    users.users.tzbot = {
      group = "tzbot";
      isSystemUser = true;
    };
    users.groups.tzbot = {};
  };
}
