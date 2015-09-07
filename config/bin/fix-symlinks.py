#!env python

import argparse
import json
import os
import shutil


def main():
    config = parse_command_line()
    command = SymlinkChecker(config)
    command.run()


def parse_command_line():
    parser = argparse.ArgumentParser(
        description="Check and update symlinks for configuration files")
    parser.add_argument(
        '--config', '-c', dest='symlink_d',
        default='~/.nix-profile/etc/johbo/symlinks.d',
        help='Path to the configuration directory, default '
        '"%(default)s".')
    parser.add_argument(
        '--fix-symlinks', dest='fix_symlinks', action='store_true',
        help='Replace existing files or update existing symlinks.')
    return parser.parse_args()


class SymlinkChecker:

    _source = None
    _target = None

    def __init__(self, config):
        self._config = config

    def run(self):
        for symlink_config in self.symlink_configs():
            self.check_and_fix_symlinks(symlink_config)
            self.apply_create_as_copy(symlink_config)
            self.apply_directories(symlink_config)

    def symlink_configs(self):
        symlink_d = self._expand_path(self._config.symlink_d)
        files = os.listdir(symlink_d)
        full_paths = (os.path.join(symlink_d, f) for f in files)
        return (SymlinkConfig(p) for p in full_paths)

    def check_and_fix_symlinks(self, symlink_config):
        should_fix = self._config.fix_symlinks
        for symlink in symlink_config.symlinks:
            needs_fix = self._check_one_symlink(symlink)
            if should_fix and needs_fix:
                self._fix_one_symlink(symlink)

    def _check_one_symlink(self, symlink):
        self._source, self._target = symlink
        self._needs_fix = False
        self._verify_source_is_symlink()
        self._verify_target_exists()
        return self._needs_fix

    def _verify_source_is_symlink(self):
        source_expanded = self._expand_path(self._source)
        if not os.path.islink(source_expanded):
            self._needs_fix = True
            self.log_problem(
                '"{source}" should be a symbolic link, it should '
                'point to "{target}".')

    def _verify_target_exists(self):
        target_expanded = self._expand_path(self._target)
        if not os.path.exists(target_expanded):
            self._needs_fix = True
            self.log_problem('"{target}" does not exist.')

    def _expand_path(self, path):
        return os.path.expanduser(path)

    def _fix_one_symlink(self, symlink):
        source_expanded = self._expand_path(self._source)
        target_expanded = self._expand_path(self._target)
        if os.path.exists(source_expanded):
            os.remove(source_expanded)
        os.symlink(target_expanded, source_expanded)
        self.log_change(
            'Created symlink "{source}" pointing to "{target}".')

    def apply_create_as_copy(self, config_data):
        for item in config_data.create_as_copy:
            self._create_one_copy(item)

    def _create_one_copy(self, item):
        self._target, self._source = item
        if not self._config.fix_symlinks:
            self.log_info('Would copy "{source}" to "{target}".')
            return

        self.log_change('Copying from "{source}" to "{target}".')
        try:
            shutil.copyfile(
                self._expand_path(self._source),
                self._expand_path(self._target))
        except IOError as e:
            self.log_problem('Copy operation failed: {reason}', reason=e)

    def apply_directories(self, config_data):
        for item in config_data.directories:
            self._create_one_directory(item)

    def _create_one_directory(self, dir_path):
        dir_path = self._expand_path(dir_path)

        if os.path.exists(dir_path):
            return

        if not self._config.fix_symlinks:
            self.log_info(
                'Would create directory "{dir_path}".', dir_path=dir_path)
            return

        self.log_change('Creating directory "{dir_path}".', dir_path=dir_path)
        try:
            os.makedirs(dir_path)
        except IOError as e:
            self.log_problem(
                'Creating the directory failed: {reason}', reason=e)

    def log_info(self, message_template, **params):
        self.log_message('INFO', message_template, **params)

    def log_change(self, message_template, **params):
        self.log_message('CHANGE', message_template, **params)

    def log_problem(self, message_template, **params):
        self.log_message('WARNING', message_template, **params)

    def log_message(self, category, message_template, **params):
        params = params.copy()
        params.update({'source': self._source, 'target': self._target})
        message = message_template.format(**params)
        print(category + ': ' + message)


class SymlinkConfig:

    def __init__(self, path):
        self._path = path
        self._load_data()

    def _load_data(self):
        with open(self._path, 'rb') as symlinks_file:
            self._data = json.load(symlinks_file)

    @property
    def symlinks(self):
        return self._data.get('symlinks', [])

    @property
    def create_as_copy(self):
        return self._data.get('create_as_copy', [])

    @property
    def directories(self):
        return self._data.get('directories', [])


if __name__ == '__main__':
    main()
