policy('Signals Access Policy','Signals Access', [
        user('Sebastian'),
        user('Ana'),
        user('OEM employee 1'),

        user_attribute('Vehicle Owners'),
        user_attribute('Vehicle OEM'),

        object('VIN-1001 Shift Signals'),
        object('VIN-1001 Window Signals'),
        object('VIN-1001 Door Signals'),
        object('VIN-1001 Trip Signals'),

        object('VIN-1002 Shift Signals'),
        object('VIN-1002 Window Signals'),
        object('VIN-1002 Door Signals'),
        object('VIN-1002 Trip Signals'),

        object('VIN-2001 Shift Signals'),
        object('VIN-2001 Window Signals'),
        object('VIN-2001 Door Signals'),
        object('VIN-2001 Trip Signals'),

        object('VIN-3001 Shift Signals'),
        object('VIN-3001 Window Signals'),
        object('VIN-3001 Door Signals'),
        object('VIN-3001 Trip Signals'),

        object_attribute('Trip Signals'),
        object_attribute('Window Signals'),
        object_attribute('Door Signals'),
        object_attribute('Shift Signals'),

        object_attribute('Owner Accessible Signals'),
        object_attribute('OEM Accessible Signals'),

        policy_class('Signals Access'),

        connector('PM'),

        assign('Sebastian', 'Vehicle Owners'),
        assign('Ana', 'Vehicle Owners'),

        assign('OEM employee 1', 'Vehicle OEM'),

        assign('Vehicle OEM', 'Vehicle Owners'),

        assign('VIN-1001 Shift Signals', 'Shift Signals'),
        assign('VIN-1001 Window Signals', 'Window Signals'),
        assign('VIN-1001 Door Signals', 'Door Signals'),
        assign('VIN-1001 Trip Signals', 'Trip Signals'),

        assign('VIN-1002 Shift Signals', 'Shift Signals'),
        assign('VIN-1002 Window Signals', 'Window Signals'),
        assign('VIN-1002 Door Signals', 'Door Signals'),
        assign('VIN-1002 Trip Signals', 'Trip Signals'),

        assign('VIN-2001 Shift Signals', 'Shift Signals'),
        assign('VIN-2001 Window Signals', 'Window Signals'),
        assign('VIN-2001 Door Signals', 'Door Signals'),
        assign('VIN-2001 Trip Signals', 'Trip Signale'),

        assign('VIN-3001 Shift Signals', 'Shift Signals'),
        assign('VIN-3001 Window Signals', 'Window Signals'),
        assign('VIN-3001 Door Signals', 'Door Signals'),
        assign('VIN-3001 Trip Signals', 'Trip Signals'),

        assign('Trip Signals', 'OEM Accessible Signals'),

        assign('Window Signals', 'Owner Accessible Signals'),
        assign('Door Signals', 'Owner Accessible Signals'),
        assign('Shift Signals', 'Owner Accessible Signals'),

        assign('Owner Accessible Signals', 'OEM Accessible Signals'),

        assign('Vehicle Owners', 'Signals Access'),
        assign('OEM Accessible Signals', 'Signals Access'),
        assign('Owner Accessible Signals', 'Signals Access'),

        assign('Signals Access','PM'),

        associate('Vehicle Owners', [r], 'Owner Accessible Signals'),
        associate('Vehicle OEM', [r,w], 'OEM Accessible Signals')
]).

