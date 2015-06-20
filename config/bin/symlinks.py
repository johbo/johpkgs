#!env python

import argparse
import json
import os


def main():
    config = parse_command_line()
    command = SymlinkChecker(config)
    command.run()


def parse_command_line():
    parser = argparse.ArgumentParser(
        description="Check and update symlinks for configuration files")
    parser.add_argument(
        '--config', '-c', dest='symlink_d',
        default='~/.nix-profile/etc/johbo/symlink.d',
        help='Path to the configuration directory, default '
        '"%(default)s".')
    return parser.parse_args()


class SymlinkChecker:

    _source = None
    _target = None

    def __init__(self, config):
        self._config = config

    def run(self):
        for symlink_config in self.symlink_configs():
            self.check_symlinks(symlink_config)

    def symlink_configs(self):
        files = os.listdir(self._config.symlink_d)
        full_paths = (
            os.path.join(self._config.symlink_d, f)
            for f in files)
        return full_paths

    def check_symlinks(self, filename):
        symlinks = self._symlinks_from_file(filename)
        for symlink in symlinks:
            self._check_one_symlink(symlink)

    def _check_one_symlink(self, symlink):
        self._source, self._target = symlink
        self._verify_source_is_symlink()
        self._verify_target_exists()

    def _verify_source_is_symlink(self):
        source_expanded = self._expand_path(self._source)
        if not os.path.islink(source_expanded):
            self.log_problem(
                '"{source}" should be a symbolic link, it should '
                'point to "{target}".')

    def _verify_target_exists(self):
        target_expanded = self._expand_path(self._target)
        if not os.path.exists(target_expanded):
            self.log_problem('"{target}" does not exist.')

    def _expand_path(self, path):
        return os.path.expanduser(path)

    def _symlinks_from_file(self, filename):
        with open(filename, 'rb') as symlinks_file:
            data = json.load(symlinks_file)
        return data['symlinks']

    def log_problem(self, message_template, **params):
        params = params.copy()
        params.update({'source': self._source, 'target': self._target})
        message = message_template.format(**params)
        print("WARNING: " + message)


if __name__ == '__main__':
    main()

