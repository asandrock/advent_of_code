program main_new
  use StringiFor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input, tmp_string
  character(len=5) :: current_hand
  type(string), allocatable :: lines(:), hand(:)
  integer :: j, k, n_hands, tmp
  integer, allocatable :: bid(:), classified(:)

  integer, parameter :: high_card = 1, one_pair = 2, two_pair = 3, &
    three_of_a_kind = 4, full_house = 5, four_of_a_kind = 6, five_of_a_kind = 7
  character(len=1), dimension(*), parameter :: cards = &
    ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_hands = size(lines)

  allocate(hand(n_hands), bid(n_hands), classified(n_hands))
  do j = 1, n_hands
    read (lines(j)%raw, '(A5,X,I8)') current_hand, bid(j)
    hand(j) = current_hand
    call classify(hand(j), classified(j))
  end do

  ! Sort hands (Bubble sort)
  do j = n_hands, 2, -1
    do k = 1, j - 1
      !if (higher(hand(k + 1), hand(k))) then
      if (classified(k + 1) > classified(k)) then
        tmp_string = hand(k + 1)
        hand(k + 1) = hand(k)
        hand(k) = tmp_string

        tmp = bid(k + 1)
        bid(k + 1) = bid(k)
        bid(k) = tmp

        tmp = classified(k + 1)
        classified(k + 1) = classified(k)
        classified(k) = tmp
      else if (classified(k + 1) == classified(k)) then
        if (second_rule(hand(k + 1), hand(k))) then
          tmp_string = hand(k + 1)
          hand(k + 1) = hand(k)
          hand(k) = tmp_string

          tmp = bid(k + 1)
          bid(k + 1) = bid(k)
          bid(k) = tmp

          tmp = classified(k + 1)
          classified(k + 1) = classified(k)
          classified(k) = tmp
        end if
      end if
    end do
  end do

  tmp = 0
  do j = n_hands, 1, -1
    !print *, n_hands - j + 1, hand(j), bid(j)
    tmp = tmp +  (n_hands - j + 1)*bid(j)
  end do
  print *, tmp
contains
  recursive subroutine classify(hand, res)
    type(string), intent(in) :: hand
    integer, intent(out) :: res

    integer :: num_joker, res_joker(size(cards) - 1), j, loc_joker
    type(string) :: joker_hand

    num_joker = hand%count('J')
    if (num_joker == 0) then
      call classify_normal(hand, res)
    else
      loc_joker = scan(hand, 'J')
      do j = 1, size(cards) - 1
        joker_hand = hand
        joker_hand%raw(loc_joker:loc_joker) = cards(j)
        call classify(joker_hand, res_joker(j))
      end do
      res = maxval(res_joker)
    end if
  end subroutine classify

  subroutine classify_normal(hand, res)
    type(string), intent(in) :: hand
    integer, intent(out) :: res

    integer, dimension(size(cards)) :: count_cards
    integer :: j

    do j = 1, size(cards)
      count_cards(j) = hand%count(cards(j))
    end do
    select case (maxval(count_cards))
    case (5)
      res = five_of_a_kind
      !print *, 'Five of a kind'
    case (4)
      res = four_of_a_kind
      !print *, 'Four of a kind'
    case (3)
      if (maxval(count_cards, mask=(count_cards < 3)) == 2) then
        res = full_house
        !print *, 'Full House'
      else
        res = three_of_a_kind
        !print *, 'Three of a kind'
      end if
    case (2)
      if (count(count_cards == 2) == 2) then
        res = two_pair
        !print *, 'Two pair'
      else
        res = one_pair
        !print *, 'One pair'
      end if
    case (1)
      res = high_card
      !print *, 'High Card'
    end select
  end subroutine classify_normal

  function second_rule(hand1, hand2)
    type(string), intent(in) :: hand1, hand2
    logical :: second_rule

    integer :: j, loc1(1), loc2(1)

    second_rule = .false.
    do j = 1, 5
      if (hand1%slice(j, j) /= hand2%slice(j, j)) then
        loc1 = findloc(cards, hand1%slice(j, j))
        loc2 = findloc(cards, hand2%slice(j, j))
        second_rule = (loc1(1) < loc2(1))
        return
      end if
    end do
  end function second_rule

  function higher(hand1, hand2)
    ! hand1 higher than hand2
    type(string), intent(in) :: hand1, hand2
    logical :: higher

    integer :: res1, res2

    call classify(hand1, res1)
    call classify(hand2, res2)
    if (res1 /= res2) then
      higher = res1 > res2
    else
      higher = second_rule(hand1, hand2)
    end if
  end function higher
end program main_new
