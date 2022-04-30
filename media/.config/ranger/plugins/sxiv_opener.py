import ranger.api
# https://github.com/hut/ranger/pull/205#issuecomment-70960474
# allows sxiv opener to work with rifle commands that don't start with "sxiv "
# also for pqiv

old_hook_ready = ranger.api.hook_ready

def hook_ready(self):
    # "self" is now the FileManager instance
    old_preprocessing_hook = self.rifle.hook_command_preprocessing

    def sxiv_workaround_hook_modified(command):
        import re
        from ranger.ext.shell_escape import shell_quote

        if self.settings.open_all_images and \
                len(self.thisdir.marked_items) == 0 and \
                re.match(r'^(.*sxiv|feh|imv|.*pqiv|.*mvi) ', command):

            images = [f.relative_path for f in self.thisdir.files if f.image]
            media = [f.relative_path for f in self.thisdir.files if f.image
                     or f.video]
            escaped_filenames = " ".join(shell_quote(f) \
                    for f in media if "\x00" not in f)

            if (images and self.thisfile.relative_path in images) or \
                    (media and self.this)and \
                    "$@" in command:
                new_command = None

                if 'sxiv ' in command:
                    number = images.index(self.thisfile.relative_path) + 1
                    new_command = command.replace("sxiv ",
                            "sxiv -n %d " % number, 1)

                if command[0:4] == 'feh ':
                    new_command = command.replace("feh ",
                        "feh --start-at %s " % \
                        shell_quote(self.thisfile.relative_path), 1)

                if command[0:4] == 'imv ':
                    number = images.index(self.thisfile.relative_path) + 1
                    new_command = command.replace("imv ",
                            "imv -n %d " % number, 1)

                if 'pqiv ' in command:
                    number = media.index(self.thisfile.relative_path)
                    new_command = command.replace("pqiv ",
                            "pqiv --action \"goto_file_byindex(%d)\" " % \
                            number, 1)

                if 'mvi ' in command:
                    number = media.index(self.thisfile.relative_path)
                    new_command = command.replace("mvi ",
                            "mvi --playlist-start=%d " % \
                            number, 1)

                if new_command:
                    command = "set -- %s; %s" % (escaped_filenames,
                            new_command)
        return old_preprocessing_hook(command)

    self.rifle.hook_command_preprocessing = sxiv_workaround_hook_modified

ranger.api.hook_ready = hook_ready
