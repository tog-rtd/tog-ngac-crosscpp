policy('Vehicle Ownership Policy','Vehicle Ownership', [
	user('Sebastian'),
        user('Ana'),

	user_attribute('Scholze Family'),
        user_attribute('Correia Family'),

        user_attribute('Owners'),

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

	object_attribute('Vehicle VIN-1001'),
	object_attribute('Vehicle VIN-1002'),
	object_attribute('Vehicle VIN-2001'),
	object_attribute('Vehicle VIN-3001'),

	object_attribute('Scholze Family Vehicles'),
	object_attribute('Correia Family Vehicles'),

	object_attribute('Vehicles'),

	policy_class('Vehicle Ownership'),

	connector('PM'),

	assign('Sebastian', 'Scholze Family'),
	assign('Ana', 'Correia Family'),

	assign('Scholze Family', 'Owners'),
	assign('Correia Family', 'Owners'),

	assign('VIN-1001 Shift Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Window Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Door Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Trip Signals', 'Vehicle VIN-1001'),

	assign('VIN-1002 Shift Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Window Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Door Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Trip Signals', 'Vehicle VIN-1002'),

	assign('VIN-2001 Shift Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Window Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Door Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Trip Signals', 'Vehicle VIN-2001'),

	assign('VIN-3001 Shift Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Window Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Door Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Trip Signals', 'Vehicle VIN-3001'),

	assign('Vehicle VIN-1001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-1002', 'Correia Family Vehicles'),
	assign('Vehicle VIN-2001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-3001', 'Correia Family Vehicles'),

	assign('Scholze Family Vehicles', 'Vehicles'),
	assign('Correia Family Vehicles', 'Vehicles'),

	assign('Owners', 'Vehicle Ownership'),
	assign('Vehicles', 'Vehicle Ownership'),

	assign('Vehicle Ownership', 'PM'),

	associate('Scholze Family',[o,r],'Scholze Family Vehicles'),
	associate('Correia Family',[o,r],'Correia Family Vehicles')
	]).
