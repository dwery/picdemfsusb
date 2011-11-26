#!/usr/bin/perl -w

use Device::USB;
use IO::File;
#use Data::HexDump;

	use constant READ_VERSION	=> 0x00;
	use constant READ_FLASH		=> 0x01;
	use constant WRITE_FLASH	=> 0x02;
	use constant ERASE_FLASH	=> 0x03;
	use constant READ_EEDATA	=> 0x04;
	use constant WRITE_EEDATA	=> 0x05;
	use constant READ_CONFIG	=> 0x06;
	use constant WRITE_CONFIG	=> 0x07;
	use constant UPDATE_LED		=> 0x32;
	use constant RESET		=> 0xFF;

	use constant PICDEM_VID		=> 0x04D8;
	use constant PICDEM_PID		=> 0x000B;

	use constant ERASE_NONE		=> 0x00;
	use constant ERASE_AUTO		=> 0x01;
	use constant ERASE_SMART	=> 0x02;

	use constant MAX_ERRORS		=> 10;

	my $commands = {

		'dump'		=> [ qw ( userid config eeprom program ) ],
		'erase'		=> [ qw ( userid eeprom program auto smart ) ],
		'compare'	=> 'file',
		'reset'		=> undef,
		'fill'		=> undef,
		'write'		=> 'file',
		'wait'		=> undef,
		'leds'		=> undef,
	};

	print "picdem.pl 1.0 - 20111126\n";
	print "Copyright (C) 2005-11 Tower Technologies, written by Alessandro Zummo,\n";
	print "licensed under the GPL. Feedback to a.zummo\@towertech.it.\n\n";

	# show usage when no args
	exitwhelp() unless scalar @ARGV;

	# validate commands
	my @cmds = @ARGV;

	while (scalar @cmds)
	{
		my $cmd = shift @cmds;

		# check command
		exitwhelp("Unknown command: $cmd\n")
			unless grep { $cmd eq $_ } keys %$commands;

		my $what = shift @cmds
			if defined $commands->{$cmd};

		# check command's option (if required)
		exitwhelp(
			"Unknown option for $cmd: ",
			defined $what ? $what : '<none given>',
			"\n",
			"Valid options are: ",
			join(',', @{$commands->{$cmd}}), "\n"
		)
		if ref($commands->{$cmd}) eq 'ARRAY'
		and not grep { defined $what and $what eq $_ } @{$commands->{$cmd}};

		exitwhelp("Missing filename\n")
			if defined $commands->{$cmd}
			and $commands->{$cmd} eq 'file'
			and not defined $what;
	}

	my $erase_mode = ERASE_NONE;

	# Init USB, search for 0x04D8/0x000B
	my $usb = new Device::USB;
	my $dev = $usb->find_device(PICDEM_VID, PICDEM_PID);

	if ($ARGV[0] eq 'wait') {
		print "Awaiting PICDEM USB to appear on the bus..\n";

		do {
			$dev = $usb->find_device(PICDEM_VID, PICDEM_PID);
		} while (not defined $dev);
	}

	die "Couldn't find PICDEM FS USB\n"
		unless defined $dev;

	print "PICDEM FS USB found\n";

	# open and setup
	$dev->open;

	$dev->set_configuration(1);
	$dev->claim_interface(0);

	# banner
	my ($maj, $min) = picdem_version($dev);
	print "bootloader version $maj.$min\n\n";

	# led on
	picdem_update_led($dev, 4, 1);

	my $cmd = '';

	while (scalar @ARGV) {
		# grab cmd
		$cmd = shift;

		# grab argument, if required.
		my $what = shift
			if defined $commands->{$cmd};

		job($dev, $cmd, $what);
	}

	# leds off
	# (unless the last command was a reset).

	if ($cmd ne 'reset') {
		picdem_update_led($dev, 3, 0);
		picdem_update_led($dev, 4, 0);
	}

	$dev->release_interface(0);
	undef $dev;

	print "done.\n";

sub job
{
	my ($handle, $cmd, $what) = @_;

	# handle commands

	print $cmd;
	print ' ', $what if defined $what;
	print "\n";

	if ($cmd eq 'dump') {
		picdem_dump_userid($handle)	if $what eq 'userid';
		picdem_dump_config($handle)	if $what eq 'config';
		picdem_dump_eeprom($handle)	if $what eq 'eeprom';
		picdem_dump_flash($handle)	if $what eq 'program';
	}

	if ($cmd eq 'erase') {
		picdem_erase_eeprom($handle)	if $what eq 'eeprom';
		picdem_erase_flash($handle)	if $what eq 'program';
		picdem_erase_block($handle, 0x00200000, 1)
						if $what eq 'userid';
		$erase_mode = ERASE_AUTO	if $what eq 'auto';
		$erase_mode = ERASE_SMART	if $what eq 'smart';
	}

	if ($cmd eq 'write') {
		my $data = load_inhx32($what);
		print "loaded regions: ", join(',', keys %{$data->{'region'}}), "\n";

		if ($erase_mode == ERASE_AUTO) {
			foreach my $region (keys %{$data->{'region'}}) {
				job($handle, 'erase', $region);
			}
		}

		print "writing...\n";
		picdem_write_flash($handle, $data);

		print "comparing...\n";
		picdem_compare_flash($handle, $data);
	}

	picdem_compare_flash($handle, load_inhx32($what))
				if $cmd eq 'compare';

	picdem_reset($handle)	if $cmd eq 'reset';
	picdem_fill($handle)	if $cmd eq 'fill';
	picdem_leds($handle)	if $cmd eq 'leds';

	print "\n";
}

sub exitwhelp
{
	print @_;

	print "\nUsage:\n\n";

	foreach (sort keys %$commands) {
		print "$_\t[", join(',', @{$commands->{$_}}), "]\n"
			if defined $commands->{$_}
			and ref($commands->{$_}) eq 'ARRAY';

		print "$_\t<inhx32 file>[,<file>,]\n"
			if defined $commands->{$_}
			and $commands->{$_} eq 'file';

		print "$_\n"
			if not defined $commands->{$_};
	}

	exit;
}

sub do_cmd
{
	my ($handle, $command, @data) = @_;

	my ($out, $len) = encode($command, @data);
	my $in = 0xFF x 64;

#	print ">>> $len\n";
#	dump_buffer($out);

	my $num = $dev->bulk_write(0x01, $out, $len, 1000);

	return if $command == RESET;

	die "usb_bulk_write: $!\n"
		if $num < 0;

	$num = $dev->bulk_read(0x81, $in, 64, 500);
	die "usb_bulk_read: $!\n"
		if $num < 0;

#	print "<<< $num\n";
#	dump_buffer(substr($in, 0, $num));

	@data = unpack('C*', substr($in, 0, $num));

	return \@data;
}

sub encode
{
	my ($command, @data) = @_;

	my $raw = pack('C*', $command, @data);

	return ($raw, length($raw));
}
	
sub dump_buffer
{
	my ($buffer) = @_;

	my ($cmd, $len, $addr1, $addr2, $addr3, $data)  = unpack('C C C C C a*', $buffer);

#	printf "CMD: %02x\n", $cmd;
#	printf "LEN: %02x\n", $len;

	print Data::HexDump::HexDump($buffer);

	print "\n";
}

sub picdem_version
{
	my ($handle) = @_;

	my $data = do_cmd($handle, READ_VERSION, 0x00, 0x00, 0x00, 0x00);	

	return ($data->[3], $data->[2]);
}

sub picdem_leds
{
	my ($dev) = @_;


	print "D3 on, D4 on\n";

	picdem_update_led($dev, 3, 1);
	picdem_update_led($dev, 4, 1);

	sleep 1;

	print "D3 off, D4 on\n";

	picdem_update_led($dev, 3, 0);
	picdem_update_led($dev, 4, 1);

	sleep 1;

	print "D3 on, D4 off\n";

	picdem_update_led($dev, 3, 1);
	picdem_update_led($dev, 4, 0);

	sleep 1;
}

sub picdem_reset
{
	my ($handle) = @_;

	do_cmd($handle, RESET, 0x00, 0x00, 0x00, 0x00);
}

sub picdem_update_led
{
	my ($handle, $led, $state) = @_;

	do_cmd($handle, UPDATE_LED, $led, $state);
}

sub picdem_dump_config
{
	my ($handle) = @_;

	my $cfg1 = do_cmd($handle, READ_CONFIG, 14, 0x00, 0x00, 0x30);
	my $cfg2 = do_cmd($handle, READ_CONFIG, 2, 0xFE, 0xFF, 0x3F);

	$cfg1 = join ' ', map { sprintf "%02X", $_ } splice @$cfg1, 5;

	print "\n";
	printf "300000: %s\n", $cfg1;
	printf "3000FE: %02X\n", $cfg2->[5];
	printf "3000FF: %02X\n", $cfg2->[6];
	print "\n";
}

sub picdem_dump_userid
{
	my ($handle) = @_;

	my $uid = do_cmd($handle, READ_CONFIG, 8, 0x00, 0x00, 0x20);

	$uid = join ' ', map { sprintf "%02X", $_ } splice @$uid, 5;

	print "\n";
	print "200000: $uid\n";
	print "\n";
}

sub picdem_dump_eeprom
{
	my ($handle) = @_;

	print "\n";

	for (my $i = 0; $i < 0xFF; $i += 16) {
		my $mem = do_cmd($handle, READ_EEDATA, 16, mkpicaddr($i));

		$mem = join ' ', map { sprintf "%02X", $_ } splice @$mem, 5;

		printf "0000%02X: $mem\n", $i;
	}

	print "\n";
}

sub picdem_erase_eeprom
{
	my ($handle) = @_;

	for (my $i = 0; $i < 0xFF; $i += 16) {
		do_cmd($handle, WRITE_EEDATA, 16,
			mkpicaddr($i),
			(0xFF) x 16);
	}
}

sub mkpicaddr
{
	my $addr = $_[0];

	return (
		($addr & 0x0000FF),
		(($addr & 0x00FF00) >> 8),
		(($addr & 0xFF0000) >> 16),
	);
}


sub picdem_dump_flash
{
	my ($handle) = @_;

	print "program:\n";

	for (my $i = 0; $i < 0x007FFF; $i += 16) {
		my @addr = mkpicaddr($i);

		my $mem = do_cmd($handle, READ_FLASH, 16, @addr);

		$mem = join ' ', map { sprintf "%02X", $_ } splice @$mem, 5;

		printf "%02X%02X%02X: $mem\n", $addr[2], $addr[1], $addr[0];
	}

	print "\n";
}

# erases $count 64-byte blocks starting @ $addr

sub picdem_erase_block
{
	my ($handle, $addr, $count) = @_;

	do_cmd($handle, ERASE_FLASH, $count, mkpicaddr($addr));
}

sub picdem_erase_flash
{
	my ($handle) = @_;

#	optimized but not working.. (??)
#	picdem_erase_block($handle, 0x00000800, 0xE0);
#	picdem_erase_block($handle, 0x00004000, 0xFF);

	for (my $i = 0x800; $i < 0x7FFF; $i += 64) {
		picdem_erase_block($handle, $i, 1);
	}
}

sub picdem_read
{
	my ($handle, $addr, $count) = @_;

	my @result = ();

	#printf "picdem_read: %08X %d\n", $addr, $count;

	while ($count > 0) {
		my $mem;
		my $len = $count > 16 ? 16 : $count;

		#printf "picdem_read| %08X %d\n", $addr, $len;

		if ($addr >= 0x00F00000 and $addr <= 0x00F000FF) {
			$mem = do_cmd($handle, READ_EEDATA, $len, mkpicaddr($addr & 0xFF));
		} else {
			$mem = do_cmd($handle, READ_FLASH, $len, mkpicaddr($addr));
		}

		push(@result, splice(@$mem, 5));

		$count -= 16;
		$addr += 16;
	}

	#printf "picdem_read| read %d bytes\n", scalar @result;
	#print join ' ', map { sprintf "%02X", $_ } @result;
	#print "\n";

	return @result;
}

sub picdem_compare_flash
{
	my ($handle, $data) = @_;

	my $errors = 0;

	foreach my $addr (sort keys %{$data->{'addr'}}) {
		# skip optimized ranges
		next if defined $data->{'addr'}{$addr}{'optimized'};

		# avoid overwriting the bootloader
		next unless $addr >= 0x800;

		my @data = unpack('C*', $data->{'addr'}{$addr}{'data'});
		my $len = scalar @data;

		my @read = picdem_read($handle, $addr, $len);

		for (my $i = 0; $i < $len; $i++) {
		#for (0 .. $len - 1) {
			printf "mismatch #%d @ %08X: wanted %02X, read %02X\n",
				++$errors, $addr + $i,
				$data[$i], $read[$i]
			unless $data[$i] == $read[$i];
		}

		last if $errors > MAX_ERRORS;
	}

	print "comparison succesful\n"
		unless $errors > 0;

	print "aborted after $errors errors\n"
		if $errors > MAX_ERRORS;

	print "comparison unsuccessful\n"
		if $errors;
}

sub picdem_write
{
	my ($handle, $addr, @data) = @_;

	my $count = scalar @data;

	# picdem seems to only accept 16 bytes packets. We extend
	# the packet with 0xFF. It will not hurt.
	push(@data, (0xFF) x (16 - $count)), $count = 16
		if $count < 16;

	die "picdem_write: byte count must be a multiple of 16\n"
		unless $count % 16 == 0;

	while ($count > 0) {
		if ($addr >= 0x00F00000 and $addr <= 0x00F000FF) {
			# eeprom
			do_cmd($handle, WRITE_EEDATA, 16,
				mkpicaddr($addr & 0x0000FF),
				@data);
		} else {
			# flash
			do_cmd($handle, WRITE_FLASH, 16,
				mkpicaddr($addr), @data);
		}

		$count -= 16;
		$addr += 16;
	}
}

sub compare_arrays
{
	my ($first, $second) = @_;
	no warnings;  # silence spurious -w undef complaints

	return 0
		unless @$first == @$second;

	for (my $i = 0; $i < @$first; $i++) {
		return 0
			if $first->[$i] ne $second->[$i];
	}

	return 1;
}

sub picdem_write_flash
{
	my ($handle, $data) = @_;

	my $wrote = 0;

	foreach my $addr (sort keys %{$data->{'addr'}}) {
		next
			if defined $data->{'addr'}{$addr}{'optimized'};

		# protect boot block. shouldn' be necessary
		# because of protection bits.. but.. who knows.
		next
			unless $addr >= 0x800;
		
		# skip configuration data. a different command
		# is required to write it and is far too dangerous
		# to be implemented. Use the original windows tool.
		next
			if $addr >= 0x300000 && $addr <= 0x30000D;

		my @addr = mkpicaddr($addr);
		my @data = unpack('C*', $data->{'addr'}{$addr}{'data'});

		my $len = scalar @data;

		# smart erase
		if ($erase_mode == ERASE_SMART) {
			my $block = $addr & 0xFFFFFFC0;

			if (not defined $data->{'erased'}{$block}) {
				$data->{'erased'}{$block} = 1;
				picdem_erase_block($handle, $block, 1);
			}
		}

		# check data is at 16 bytes boundary
		if ($addr & 0x0000000F) {
			my $block = $addr & 0xFFFFFFF0;

			#printf "fixing boundary: %08X %08X %d\n", $addr, $block, $len;
			@data = ((0xFF) x ($addr - $block), @data);

			$len = scalar @data;
			$addr = $block;
		}

		#printf "programming %08X\n", $addr;
		picdem_write($handle, $addr, @data);

		$wrote += $len;
	}

	printf "programmed %d locations\n", $wrote;
}

sub picdem_fill
{
	my ($handle) = @_;

	print "filling program memory with 0x55AA\n";

	for (my $i = 0x800; $i < 0x7FFF; $i += 16) {
		do_cmd($handle, WRITE_FLASH, 
			16, mkpicaddr($i),
			((0x55, 0xAA) x 8));
	}
}

sub load_inhx32
{
	my ($files) = @_;

	my %data = ();

	foreach my $file (split(/,/, $files)) {
		my $fh = new IO::File;

		$fh->open($file, '<:crlf')
			or die "Couldn't open $file: $!\n";

		print "loading $file\n";

		my $high = 0x0000;

		while (<$fh>) {
			next unless /^:([[:xdigit:]]{2})([[:xdigit:]]{4})([[:xdigit:]]{2})([[:xdigit:]]{0,}?)([[:xdigit:]]{2})$/;

			my ($count, $addr, $type, $data, $checksum) = ($1, hex($2), $3, $4, $5);

			last
				if $type == 0x01; # EOF

			# Extended linear address record
			if ($type == 0x04) {
				die "Wrong address in type 0x04, must be 0x0000, found $addr\n"
					unless $addr == 0x0000;

				$high = (hex($data) << 16);
				next;
			}
			# Extended segment address record
			if ($type == 0x02) {
				printf "h:%08X c:$count a:%08X t:$type d:$data c:$checksum\n", $high, $addr;

				die "Wrong address in type 0x02, must be 0x0000, found $addr\n"
					unless $addr == 0x0000;

				$high = (hex($data) << 4);
				next;
			}

			# Data record
			if ($type == 0x00) {
				#printf "h:%08X c:$count a:%08X t:$type d:$data c:$checksum\n", $high, $addr;

				$addr = $high | $addr;

				$data{'addr'}{$addr}{'data'} = pack('H*', $data);
				$data{'addr'}{$addr}{'length'} = length($data{'addr'}{$addr}{'data'});
				$data{'addr'}{$addr}{'checksum'} = pack('H', $checksum);

				$data{'region'}{'program'} = 1 if $addr >= 0x0000 and $addr <= 0x7FFF;
				$data{'region'}{'userid'} = 1 if $addr >= 0x200000 and $addr <= 0x200007;
				$data{'region'}{'eeprom'} = 1 if $addr >= 0x00F00000 and $addr <= 0x00F000FF;

				next;
			}

			die "I'm not able to handle INHX32 record type $type\n";
		}

		$fh->close;
	}

#	print Data::Dumper->Dump([\%data]);

	return \%data;
}
